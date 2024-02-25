/+  telia=lia-parser
:-  %say  |=  *  :-  %noun
::
%-  main:telia
'''
string0 = [11 121404708502361365413651784]
retptr = __wbindgen_add_to_stack_pointer(-16)
ptr0 = __wbindgen_malloc(string0.len, 1)
memory.write(string0, ptr0)
process(retptr, ptr0, len0)
r0 = memory.read(retptr, 4)
r1 = memory.read(retptr+4, 4)
memory.read(r0, r1)
'''