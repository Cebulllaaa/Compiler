import os

def getRes(args):
    getn = 0
    get = 0
    ans = [""] *3
    it =0
    print("Rozpoczynam pobieranie argumentow")
    for i in args:
        if get == 1 and i == "\n":
            it = it+1
            get =0
        elif get ==1:
           ans[it] = ans[it] + i
        if getn ==0 and i == ">":
            getn =1
        elif getn == 1:
            get =1
            getn =0
    return ans
            
command = "bin/Compiler Tests/simpleIf.imp TestsResults/simpleIf.mr"
os.system(command)
command = "bin/interpreter TestsResults/simpleIf.mr"
results = getRes(os.popen(command).read())
for r in  results:
    if( r != ""):
        print(r)
