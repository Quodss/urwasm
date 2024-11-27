::  Mapping of opcodes of SIMD instructions with
::  no immediate arguments to $instructions.
::
/-  *wasm
|=  op=@
~+
|^
^-  (unit instruction)
=;  =(unit instr-vec)
  ?~  unit  ~
  `[%vec u.unit]
?+  op    ~
  %14   `[%swizzle ~]
  %98   `[%popcnt ~] 
  %77   `[%not ~]
  %78   `[%and ~]
  %79   `[%andnot ~]
  %80   `[%or ~]
  %81   `[%xor ~]
  %82   `[%bitselect ~]
  %83   `[%any-true ~]
  %130  `[%q15mul-r-sat ~]
  %186  `[%dot ~] 
  %94   `[%demote ~]
  %95   `[%promote ~]
::
  ?(%106 %148)  `(nearest op)
  ?(%227 %239)  `(sqrt op)
  ?(%231 %243)  `(div op)
  ?(%234 %246)  `(pmin op)
  ?(%235 %247)  `(pmax op)
  ?(%123 %155)  `(avgr op)
  ?(%103 %116)  `(ceil op) 
  ?(%104 %117)  `(floor op)
::
  ?(%99 %131 %163 %195)   `(all-true op)
  ?(%100 %132 %164 %196)  `(bitmask op) 
  ?(%101 %102 %133 %134)  `(narrow op)
  ?(%107 %139 %171 %203)  `(shl op)
  ?(%124 %125 %126 %127)  `(extadd op) 
  ?(%250 %251 %254 %255)  `(convert op)
::
  ?(%149 %181 %213 %230 %242)       `(mul op) 
  ?(%15 %16 %17 %18 %19 %20)        `(splat op)
  ?(%35 %45 %55 %214 %65 %71)       `(eq op)
  ?(%36 %46 %56 %215 %66 %72)       `(ne op)
  ?(%96 %128 %160 %192 %224 %236)   `(abs op)
  ?(%97 %129 %161 %193 %225 %237)   `(neg op) 
  ?(%105 %122 %248 %249 %252 %253)  `(trunc op) 
::
  ?(%37 %38 %47 %48 %57 %58 %216 %67 %73)  `(lt op)
  ?(%39 %40 %49 %50 %59 %60 %217 %68 %74)  `(gt op)
  ?(%41 %42 %51 %52 %61 %62 %218 %69 %75)  `(le op)
  ?(%43 %44 %53 %54 %63 %64 %219 %70 %76)  `(ge op)
::
  ?(%108 %109 %140 %141 %172 %173 %204 %205)  `(shr op)
  ?(%118 %119 %150 %151 %182 %183 %232 %244)  `(min op)
  ?(%120 %121 %152 %153 %184 %185 %233 %245)  `(max op)
::
  ?(%110 %111 %112 %142 %143 %144 %174 %206 %228 %240)  `(add op) 
  ?(%113 %114 %115 %145 %146 %147 %177 %209 %229 %241)  `(sub op) 
::
  ?(%135 %136 %137 %138 %167 %168 %169 %170 %199 %200 %201 %202)  `(extend op)
  ?(%156 %157 %158 %159 %188 %189 %190 %191 %220 %221 %222 %223)  `(extmul op)
==
::
++  nearest
  |=  op=?(%106 %148)
  ^-  instr-vec
  :-  %nearest
  ?-  op
    %106  %f32
    %148  %f64
  ==
::
++  sqrt
  |=  op=?(%227 %239)
  ^-  instr-vec
  :-  %sqrt
  ?-  op
    %227  %f32
    %239  %f64
  ==
::
++  div
  |=  op=?(%231 %243)
  ^-  instr-vec
  :-  %div
  ?-  op
    %231  %f32
    %243  %f64
  ==
::
++  pmin
  |=  op=?(%234 %246)
  ^-  instr-vec
  :-  %pmin
  ?-  op
    %234  %f32
    %246  %f64
  ==
::
++  pmax
  |=  op=?(%235 %247)
  ^-  instr-vec
  :-  %pmax
  ?-  op
    %235  %f32
    %247  %f64
  ==
::
++  avgr
  |=  op=?(%123 %155)
  ^-  instr-vec
  :-  %avgr
  :_  %u
  ?-  op
    %123  %i8
    %155  %i16
  ==
::
++  ceil
  |=  op=?(%103 %116)
  ^-  instr-vec
  :-  %ceil
  ?-  op
    %103  %f32
    %116  %f64
  ==
::
++  floor
  |=  op=?(%104 %117)
  ^-  instr-vec
  :-  %floor
  ?-  op
    %104  %f32
    %117  %f64
  ==
::
++  all-true
  |=  op=?(%99 %131 %163 %195)
  ^-  instr-vec
  :-  %all-true
  ?-  op
    %99   %i8
    %131  %i16
    %163  %i32
    %195  %i64
  ==
::
++  bitmask 
  |=  op=?(%100 %132 %164 %196)
  ^-  instr-vec
  :-  %bitmask
  ?-  op
    %100  %i8
    %132  %i16
    %164  %i32
    %196  %i64
  ==
::
++  narrow
  |=  op=?(%101 %102 %133 %134)
  ^-  instr-vec
  :-  %narrow
  ?-  op
    %101  [%i8 %s]
    %102  [%i8 %u]
    %133  [%i16 %s]
    %134  [%i16 %u]
  ==
::
++  shl
  |=  op=?(%107 %139 %171 %203)
  ^-  instr-vec
  :-  %shl
  ?-  op
    %107  %i8
    %139  %i16
    %171  %i32
    %203  %i64
  ==
::
++  extadd 
  |=  op=?(%124 %125 %126 %127)
  ^-  instr-vec
  :-  %extadd
  ?-  op
    %124  [%i16 %s]
    %125  [%i16 %u]
    %126  [%i32 %s]
    %127  [%i32 %u]
  ==
::
++  convert
  |=  op=?(%250 %251 %254 %255)
  ^-  instr-vec
  :-  %convert
  ?-  op
    %250  [%f32 %s]
    %251  [%f32 %u]
    %254  [%f64 %s]
    %255  [%f64 %u]
  ==
::
++  mul 
  |=  op=?(%149 %181 %213 %230 %242)       
  ^-  instr-vec
  :-  %mul
  ?-  op
    %149  %i16
    %181  %i32
    %213  %i64
    %230  %f32
    %242  %f64
  ==
::
++  splat
  |=  op=?(%15 %16 %17 %18 %19 %20)        
  ^-  instr-vec
  :-  %splat
  ?-  op
    %15  %i8
    %16  %i16
    %17  %i32
    %18  %i64
    %19  %f32
    %20  %f64
  ==
::
++  eq
  |=  op=?(%35 %45 %55 %214 %65 %71)       
  ^-  instr-vec
  :-  %eq
  ?-  op
    %35   %i8
    %45   %i16
    %55   %i32
    %214  %i64
    %65   %f32
    %71   %f64
  ==
::
++  ne
  |=  op=?(%36 %46 %56 %215 %66 %72)       
  ^-  instr-vec
  :-  %ne
  ?-  op
    %36   %i8
    %46   %i16
    %56   %i32
    %215  %i64
    %66   %f32
    %72   %f64
  ==
::
++  abs
  |=  op=?(%96 %128 %160 %192 %224 %236)   
  ^-  instr-vec
  :-  %abs
  ?-  op
    %96   %i8
    %128  %i16
    %160  %i32
    %192  %i64
    %224  %f32
    %236  %f64
  ==
::
++  neg 
  |=  op=?(%97 %129 %161 %193 %225 %237)   
  ^-  instr-vec
  :-  %neg
  ?-  op
    %97   %i8
    %129  %i16
    %161  %i32
    %193  %i64
    %225  %f32
    %237  %f64
  ==
::
++  trunc
  |=  op=?(%105 %122 %248 %249 %252 %253)  
  ^-  instr-vec
  :-  %trunc
  ?-  op
    %105  [%f32 %f32 %s]
    %122  [%f64 %f64 %s]
    %248  [%i32 %f32 %s]
    %249  [%i32 %f32 %u]
    %252  [%i32 %f64 %s]
    %253  [%i32 %f64 %u]
  ==
::
++  lt
  |=  op=?(%37 %38 %47 %48 %57 %58 %216 %67 %73)
  ^-  instr-vec
  :-  %lt
  ?-  op
    %37   [%i8 %s]
    %38   [%i8 %u]
    %47   [%i16 %s]
    %48   [%i16 %u]
    %57   [%i32 %s]
    %58   [%i32 %u]
    %216  [%i64 %s]
    %67   [%f32 %s]
    %73   [%f64 %s]
  ==
::
++  gt
  |=  op=?(%39 %40 %49 %50 %59 %60 %217 %68 %74)
  ^-  instr-vec
  :-  %gt
  ?-  op
    %39   [%i8 %s]
    %40   [%i8 %u]
    %49   [%i16 %s]
    %50   [%i16 %u]
    %59   [%i32 %s]
    %60   [%i32 %u]
    %217  [%i64 %s]
    %68   [%f32 %s]
    %74   [%f64 %s]
  ==
::
++  le
  |=  op=?(%41 %42 %51 %52 %61 %62 %218 %69 %75)
  ^-  instr-vec
  :-  %le
  ?-  op
    %41   [%i8 %s]
    %42   [%i8 %u]
    %51   [%i16 %s]
    %52   [%i16 %u]
    %61   [%i32 %s]
    %62   [%i32 %u]
    %218  [%i64 %s]
    %69   [%f32 %s]
    %75   [%f64 %s]
  ==
::
++  ge
  |=  op=?(%43 %44 %53 %54 %63 %64 %219 %70 %76)
  ^-  instr-vec
  :-  %ge
  ?-  op
    %43   [%i8 %s]
    %44   [%i8 %u]
    %53   [%i16 %s]
    %54   [%i16 %u]
    %63   [%i32 %s]
    %64   [%i32 %u]
    %219  [%i64 %s]
    %70   [%f32 %s]
    %76   [%f64 %s]
  ==
::
++  shr
  |=  op=?(%108 %109 %140 %141 %172 %173 %204 %205)
  ^-  instr-vec
  :-  %shr
  ?-  op
    %108  [%i8 %s]
    %109  [%i8 %u]
    %140  [%i16 %s]
    %141  [%i16 %u]
    %172  [%i32 %s]
    %173  [%i32 %u]
    %204  [%i64 %s]
    %205  [%i64 %u]
  ==
::
++  min
  |=  op=?(%118 %119 %150 %151 %182 %183 %232 %244)
  ^-  instr-vec
  :-  %min
  ?-  op
    %118  [%i8 %s]
    %119  [%i8 %u]
    %150  [%i16 %s]
    %151  [%i16 %u]
    %182  [%i32 %s]
    %183  [%i32 %u]
    %232  [%f32 %s]
    %244  [%f64 %s]
  ==
::
++  max
  |=  op=?(%120 %121 %152 %153 %184 %185 %233 %245)
  ^-  instr-vec
  :-  %max
  ?-  op
    %120  [%i8 %s]
    %121  [%i8 %u]
    %152  [%i16 %s]
    %153  [%i16 %u]
    %184  [%i32 %s]
    %185  [%i32 %u]
    %233  [%f32 %s]
    %245  [%f64 %s]
  ==
::
++  add
  |=  op=?(%110 %111 %112 %142 %143 %144 %174 %206 %228 %240)
  ^-  instr-vec
  :-  %add
  ?-  op
    %110  [%i8 ~]
    %111  [%i8 ~ %s]
    %112  [%i8 ~ %u]
    %142  [%i16 ~]
    %143  [%i16 ~ %s]
    %144  [%i16 ~ %u]
    %174  [%i32 ~]
    %206  [%i64 ~]
    %228  [%f32 ~]
    %240  [%f64 ~]
  ==
::
++  sub
  |=  op=?(%113 %114 %115 %145 %146 %147 %177 %209 %229 %241)
  ^-  instr-vec
  :-  %sub
  ?-  op
    %113  [%i8 ~]
    %114  [%i8 ~ %s]
    %115  [%i8 ~ %u]
    %145  [%i16 ~]
    %146  [%i16 ~ %s]
    %147  [%i16 ~ %u]
    %177  [%i32 ~]
    %209  [%i64 ~]
    %229  [%f32 ~]
    %241  [%f64 ~]
  ==
::
++  extend
  |=  op=?(%135 %136 %137 %138 %167 %168 %169 %170 %199 %200 %201 %202)
  ^-  instr-vec
  :-  %extend
  ?-  op
    %135  [%i16 %s %low]
    %136  [%i16 %s %high]
    %137  [%i16 %u %low]
    %138  [%i16 %u %high]
    %167  [%i32 %s %low]
    %168  [%i32 %s %high]
    %169  [%i32 %u %low]
    %170  [%i32 %u %high]
    %199  [%i64 %s %low]
    %200  [%i64 %s %high]
    %201  [%i64 %u %low]
    %202  [%i64 %u %high]
  ==
::
++  extmul
  |=  op=?(%156 %157 %158 %159 %188 %189 %190 %191 %220 %221 %222 %223)
  ^-  instr-vec
  :-  %extmul
  ?-  op
    %156  [%i16 %s %low]
    %157  [%i16 %s %high]
    %158  [%i16 %u %low]
    %159  [%i16 %u %high]
    %188  [%i32 %s %low]
    %189  [%i32 %s %high]
    %190  [%i32 %u %low]
    %191  [%i32 %u %high]
    %220  [%i64 %s %low]
    %221  [%i64 %s %high]
    %222  [%i64 %u %low]
    %223  [%i64 %u %high]
  ==
::
--