import re
import ply.lex as lex
import ply.yacc as yacc
import csv
from prettytable import from_csv
import sys
fp=open("error_log.txt","w")
sys.stderr=fp
f=open("quad.txt","r")
block_list=[]
tokens = ("EOF","id", "digit","ASSIGN", "PLUS","MINUS","MUL","DIV","LT","LE","GE","GT","NE","EQ","jtrue","jfalse","jmp","temporaries","space","REST","newline")
count={"id":[0],"digit":[0],"-":[0],"+":[0],"-":[0],"*":[0],"/":[0],">":[0],"<":[0],"<=":[0],">=":[0],":=":[0],"!=":[0],"==":[0],"jtrue":[0],"jfalse":[0],"jmp":[0],"temporaries":[0],"for":[0],"while":[0]}
time_cycles={":=":3,"id":5,"digit":0,"=":0,"+":3,"-":3,"*":3,"/":3,">":4,"<":4,"<=":5,">=":5,"!=":4,"==":4,"jtrue":1,"jfalse":1,"jmp":1,"temporaries":0}
for_table = dict()
registers_used=dict()
for_cycles = 0
for_i = 0
while_table = dict()
while_i =0
while_cycles=0
flag_sub=0
subroutines_count=0
jmp_lineno=0
t_ignore=(" \t,")
t_REST='.'
def t_jmp(t):
	r"\bjmp\b"
	count["jmp"][0] +=1
	global jmp_lineno
	jmp_lineno=t.lexer.lineno
	return t
def t_jtrue(t):
	r"jtrue\b"
	count["jtrue"][0] +=1
	return t
def t_jfalse(t):
	r"\bjfalse\b"
	count["jfalse"][0] +=1
	return t
def t_temporaries(t):
	r"\btmp[0-9]+"
	count["temporaries"][0]+=1
	return t
def t_id(t):
	r"\b[a-z]+\b"
	if(t.value in registers_used):
		registers_used[t.value]=registers_used[t.value]+1
	else:
		registers_used[t.value]=1
	count["id"][0]+=1
	return t
def t_digit(t):
	r"\b[0-9]+"
	count["digit"][0]+=1
	return t
def t_ASSIGN(t):
	r":="
	count[":="][0]+=1
	return t
def t_PLUS(t):
	r"\+"
	count["+"][0]+=1
	return t
def t_MINUS(t):
	r"\-"
	count["-"][0]+=1
	return t
def t_MUL(t):
	r"\*"
	count["*"][0]+=1
	return t
def t_DIV(t):
	r"\/"
	count["/"][0]+=1
	return t
def t_LT(t):
	r"<"
	count["<"][0]+=1
	return t
def t_LE(t):
	r"\<="
	count["<="][0]+=1
	return t
def t_GT(t):
	r"\>"
	count[">"][0]+=1
	return t
def t_GE(t):
	r"\>="
	count[">="][0]+=1
	return t
def t_EQ(t):
	r"\=="
	count["=="][0]+=1
	return t
def t_NE(t):
	r"\!="
	count["!="][0]+=1
	return t
def t_newline(t):
	r"\n"
	t.lexer.lineno+=1
	global lineno
	lineno=t.lexer.lineno
	return t
def t_EOF(t):
	r"EOF"
	return t
def t_error(t):
	print("an error has occured")
def p_start(p):
	"""start : whileloop newline start		 
			 | forloop newline start
			 | ifstmt newline start
			 | statement start
			 | end"""
def p_end(p):
	"""end : """
	if(not block_list):
		block_list.append((0,lineno))
	else:
		block_list.append((block_list[len(block_list)-1][1],lineno))
	
def p_statement(p):
	"""statement : assignment
			     | arithmetic newline
			     | relation newline
			     | jump newline
			     | jumptrue newline
			     | jumpfalse newline"""

def p_re(p):
	"""re : LT
		   | GT
		   | GE
		   | LE
		   | EQ
		   | NE"""
	p[0] = p[1]
def p_relation(p):
	"""relation : re id id id
			   | re id id temporaries"""
	p[0] = p[1] 
def p_jump(p):
	"""jump : jmp digit"""
	global for_cycles
	for_cycles += 1
	global flag_sub
	global block_list
	if(flag_sub==1):
		global subroutines_count
		subroutines_count+=1
	if(not block_list):
		block_list.append((0,jmp_lineno))
	else:
		block_list.append((block_list[len(block_list)-1][1],jmp_lineno))
		
	p[0] = p[1]
def p_arsymbol(p):
	"""arsymbol : PLUS
			    | MINUS
			    | MUL
			    | DIV"""
	p[0] = p[1]
def p_assignment(p):
	"""assignment : ASSIGN temporaries check
			      | ASSIGN digit id newline"""
	global flag_sub
	flag_sub=0
	global for_cycles
	for_cycles = time_cycles["id"]
	#print(p[1],p[2])
	if(p[2]=="tmp13" and p[3]=="tmp14"):
		flag_sub=1
	p[0] = p[1]
def p_check(p):
	""" check : id newline
		  | temporaries newline"""
	p[0]=p[1]
def p_arithmetic(p):
	"""arithmetic : arsymbol id id id
			      | arsymbol id digit temporaries
			     | arsymbol id id temporaries"""
	p[0] = p[2] + p[3]
def p_ifstmt(p):
	"""ifstmt : relation newline jumptrue
			  | relation newline jumpfalse"""
		      
	global for_cycles
	for_cycles += time_cycles["<"] + time_cycles["jtrue"]
	global while_cycles
	while_cycles=time_cycles["<"]+time_cycles["jtrue"]
	p[0] = p[1]
def p_jumpfalse(p):
	"""jumpfalse : jfalse digit temporaries"""
	p[0] = p[1]
def p_jumptrue(p):
	"""jumptrue : jtrue digit temporaries"""
	p[0] = p[1]
def p_body(p):
	"""body : arithmetic newline body 
			| assignment body
			| """
	global for_cycles 
	for_cycles += time_cycles["id"]
	global while_cycles
	while_cycles+=time_cycles["id"]
def p_forloop(p):
	"""forloop : assignment ifstmt newline body jump"""
	count["for"][0] += 1
	global for_i
	for_i += 1
	global for_cycles
	for_table[for_i] = for_cycles
	global for_cycles
	for_cycles = 0
def p_iteration(p):
	"""iteration : PLUS id digit id
			  | PLUS id id id"""
def p_whileloop(p):
	"""whileloop : ifstmt newline body jump"""
	count["while"][0] += 1
	global while_i
	while_i += 1
	global while_cycles
	while_table[while_i] = while_cycles
	global while_cycles
	while_cycles = 0

	'''def p_arsymbol(p):
	       #"""arsymbol : PLUS
		#	  | MINUS
		 #         | MUL
		#	  | DIV"""
	       'arsymbol : PLUS'
	       #p[0] = p[1]'''
	"""def p_error(p):
		print("An error has occured")"""
	#lexer = lex.lex()
	#s=input("Enter a quadruple expression:")
s=f.read()
lexer = lex.lex()
parser = yacc.yacc()
parse = parser.parse(s)
	#print(parse)
print("Parse Successful")
#print(registers_used)
for key in time_cycles:
	if key in count:
		if(len(count[key]) <= 1):
			count[key].append(time_cycles[key]*count[key][0])
		else:		
			count[key][1] = (time_cycles[key]*count[key][0])
#print(count)
sum_for,sum_while = 0,0
for key in for_table:	
	sum_for += for_table[key]
for key in while_table:
	sum_while += while_table[key]
count["for"].append(sum_for)
count["while"].append(sum_while)
with open('dict.csv', 'w', newline='') as csvfile:
           writer = csv.writer(csvfile, delimiter= ' ',quotechar='|', quoting=csv.QUOTE_MINIMAL)
           writer.writerow(["token","occurences","cycles"])
           for key,value in count.items():	       
               writer.writerow([key,value[0],value[1]])
read_fault = open("dict.csv","r")
line = read_fault.readline()
for line in read_fault:
	new_line = re.sub(r"\^M","",line)
	#print(new_line)	
	line = read_fault.readline()
read_fault.close()
reader = open("dict.csv", "r")
#reader = re.sub(r"\^M","",reader)
mytable = from_csv(reader)
print("Lookup Table Population")
print(mytable)
print("Total number of Memory Accesses is ",count["id"][0])
print("Memory Accesses")
with open('register.csv', 'w', newline='') as csvfile:
	writer = csv.writer(csvfile, delimiter= ' ',quotechar='|', quoting=csv.QUOTE_MINIMAL)
	writer.writerow(["ID","Accesses"])
	for key,value in registers_used.items():
		writer.writerow([key,value])
reader_reg = open("register.csv","r")
myreg = from_csv(reader_reg)
print(myreg)	
print("For count")
with open('for.csv', 'w', newline='') as csvfile:
	writer = csv.writer(csvfile, delimiter= ' ',quotechar='|', quoting=csv.QUOTE_MINIMAL)
	writer.writerow(["Loop_No","Cycles"])
	for key,value in for_table.items():
		writer.writerow([key,value])
reader_for = open("for.csv","r")
myfor = from_csv(reader_for)
print(myfor)

print("While count")
with open('while.csv', 'w', newline='') as csvfile:
	writer = csv.writer(csvfile, delimiter= ' ',quotechar='|', quoting=csv.QUOTE_MINIMAL)
	writer.writerow(["Loop_No","Cycles"])
	for key,value in while_table.items():
		writer.writerow([key,value])
read_fault = open("while.csv","r")
line = read_fault.readline()
for line in read_fault:
	new_line = re.sub(r"\^M","",line)
	line = read_fault.readline()
read_fault.close()
reader = open("while.csv", "r")
mytable = from_csv(reader)
print(mytable)
print("-"*50)
print("Number of subroutines: ",subroutines_count)
print("-"*50)
print("FLow of Code")
print(block_list)
