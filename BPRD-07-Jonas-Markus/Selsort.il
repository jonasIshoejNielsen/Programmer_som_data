  .method public hidebysig static void  SelectionSort(int32[] arr) cil managed
  {
    // Code size       57 (0x39)
    .maxstack  4                    //4 local variables, (1 parameters, 4 local variables)
    .locals init (int32 V_0,        //0 = i, 1 = least, 3= j, 2=tmp
             int32 V_1,
             int32 V_2,
             int32 V_3)
    IL_0000:  ldc.i4.0              //push 0
    IL_0001:  stloc.0               //store 0 onto i variable
    IL_0002:  br.s       IL_0032    //jump to IL_0032

    IL_0004:  ldloc.0               //load local variable 0 / i
    IL_0005:  stloc.1               //store local variable 0 onto 1, int least = i;
    IL_0006:  ldloc.0               //load local variable 0 / i
    IL_0007:  ldc.i4.1              //push constant 1 onto stack 
    IL_0008:  add                   // add local variable 0 and constant 1
    IL_0009:  stloc.3               //store value onto variable 3 //int j = i+1
    IL_000a:  br.s       IL_001a    //branch to IL_001a

    IL_000c:  ldarg.0               //load argument 0, arr
    IL_000d:  ldloc.3               //load local variable 3, j
    IL_000e:  ldelem.i4             //load element at index j, onto the top of the stack as an int32
    IL_000f:  ldarg.0               //load argument 0, arr
    IL_0010:  ldloc.1               //load local variable 1, least
    IL_0011:  ldelem.i4             //load element at index least, onto the top of the stack as an int32
    IL_0012:  bge.s      IL_0016    //if arr[j] > arr[least] go to IL_0016

    IL_0014:  ldloc.3               //load local variable 3, j
    IL_0015:  stloc.1               //store j onto local variable 1, least = j
    IL_0016:  ldloc.3               //load local variable 3, j
    IL_0017:  ldc.i4.1              //push consant 1 onto stack
    IL_0018:  add                   //push j + 1 onto stack
    IL_0019:  stloc.3               //store it in j //j=j+=1
    IL_001a:  ldloc.3               //load variable 3 onto stack  //j
    IL_001b:  ldarg.0               //load argument 0, arr
    IL_001c:  ldlen                 //arr.length
    IL_001d:  conv.i4               //convert to 32int
    IL_001e:  blt.s      IL_000c    //if j < arr.lenght go to IL_000c

    IL_0020:  ldarg.0               //load argument 0, arr
    IL_0021:  ldloc.0               //load local variable 0, i
    IL_0022:  ldelem.i4             //load element at i, on arr[i]
    IL_0023:  stloc.2               //store in local variable 2, tmp = arr[i]
    IL_0024:  ldarg.0               //load argument 0, arr               
    IL_0025:  ldloc.0               //load local variable 0, i
    IL_0026:  ldarg.0               //load argument 0, arr     
    IL_0027:  ldloc.1               //load local variable 1, least
    IL_0028:  ldelem.i4             //load element at least, arr[least]
    IL_0029:  stelem.i4             //store it onto elelement at i, arr[i] = arr[least]
    IL_002a:  ldarg.0               //load argument 0, arr
    IL_002b:  ldloc.1               //load local variable 1, least
    IL_002c:  ldloc.2               //load local variable 2, tmp
    IL_002d:  stelem.i4             //store tmp at elelemnt at least
    IL_002e:  ldloc.0               //load local variable 0, i
    IL_002f:  ldc.i4.1              //push onto stack constant 1
    IL_0030:  add                   //add i and 1
    IL_0031:  stloc.0               //store onto 0, i++
    IL_0032:  ldloc.0             //load value at offset 0, i
    IL_0033:  ldarg.0             //load argument 0, arr
    IL_0034:  ldlen               //push lenght of arr onto stack
    IL_0035:  conv.i4             //convert lenght to int32
    IL_0036:  blt.s      IL_0004  //if stloc.0 < lenght go to  IL_0004

    IL_0038:  ret                 /return void
  } // end of method Selsort::SelectionSort