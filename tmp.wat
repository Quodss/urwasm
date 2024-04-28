(module
  (type $t0 (func (result i32)))
  (type $t1 (func (param i32 i32 i32)))
  (type $t2 (func (param i32)))
  (type $t3 (func (param i32) (result i32)))
  (type $t4 (func))
  (type $t5 (func (param i32 i32)))
  (type $t6 (func (param i64 i32)))
  (type $t7 (func (param f32 i32)))
  (type $t8 (func (param f64 i32)))
  (type $t9 (func (param v128 i32)))
  (type $t10 (func (param i32) (result i64)))
  (type $t11 (func (param i32) (result f32)))
  (type $t12 (func (param i32) (result f64)))
  (type $t13 (func (param i32) (result v128)))
  (type $t14 (func (param i32 i32 i32 i32)))
  (type $t15 (func (param i32 i32 i32) (result i32)))
  (type $t16 (func (param i32 i32 i32) (result i64)))
  (type $t17 (func (param i32 i32) (result f32)))
  (type $t18 (func (param i32 i32) (result f64)))
  (type $t19 (func (param i32 i32) (result i32)))
  (import "serf" "writeHi" (func $serf.writeHi (type $t0)))
  (import "memio" "read" (func $memio.read (type $t1)))
  (import "memio" "write" (func $memio.write (type $t1)))
  (import "lia" "printf" (func $lia.printf (type $t2)))
  (func $f4 (type $t3) (param $p0 i32) (result i32)
    (local $l1 i32) (local $l2 i32)
    (if $I0
      (i32.or
        (i32.ge_u
          (local.tee $l1
            (i32.add
              (local.get $p0)
              (global.get $g0)))
          (local.tee $l2
            (i32.mul
              (i32.const 65536)
              (memory.size))))
        (i32.ge_u
          (local.get $l1)
          (i32.const 65536)))
      (then
        (call $f5)
        (if $I1
          (i32.ge_u
            (local.tee $l1
              (i32.add
                (local.get $p0)
                (global.get $g0)))
            (i32.const 65536))
          (then
            (unreachable)))
        (if $I2
          (i32.ge_u
            (local.get $l1)
            (local.get $l2))
          (then
            (if $I3
              (i32.eq
                (memory.grow
                  (i32.add
                    (i32.div_u
                      (i32.sub
                        (local.get $l1)
                        (local.get $l2))
                      (i32.const 65536))
                    (i32.const 1)))
                (i32.const -1))
              (then
                (unreachable)))))))
    (global.get $g0)
    (global.set $g0
      (local.get $l1)))
  (func $f5 (type $t4)
    (local $l0 i32) (local $l1 i32) (local $l2 i32) (local $l3 i64) (local $l4 i32) (local $l5 i32)
    (if $I0
      (i32.ge_u
        (local.tee $l1
          (i32.add
            (i32.const 2048)
            (global.get $g0)))
        (local.tee $l0
          (i32.mul
            (i32.const 65536)
            (memory.size))))
      (then
        (if $I1
          (i32.eq
            (memory.grow
              (i32.add
                (i32.div_u
                  (i32.sub
                    (local.get $l1)
                    (local.get $l0))
                  (i32.const 65536))
                (i32.const 1)))
            (i32.const -1))
          (then
            (unreachable))
          (else
            (local.set $l0
              (i32.mul
                (i32.const 65536)
                (memory.size)))))))
    (loop $L2
      (if $I3
        (i32.ge_u
          (local.get $l2)
          (i32.const 2048))
        (then)
        (else
          (if $I4
            (i64.lt_u
              (local.tee $l3
                (i64.load align=1
                  (local.get $l2)))
              (i64.const -9223372036854775808))
            (then
              (i64.store align=1
                (i32.add
                  (global.get $g0)
                  (local.get $l2))
                (local.get $l3)))
            (else
              (if $I5
                (i32.or
                  (i32.eqz
                    (local.tee $l4
                      (i32.wrap_i64
                        (i64.sub
                          (local.get $l3)
                          (i64.const -9223372036854775808)))))
                  (i32.eq
                    (local.get $l4)
                    (i32.const -1)))
                (then
                  (i64.store align=1
                    (i32.add
                      (global.get $g0)
                      (local.get $l2))
                    (local.get $l3)))
                (else
                  (i64.store align=1
                    (i32.add
                      (global.get $g0)
                      (local.get $l2))
                    (i64.add
                      (i64.extend_i32_u
                        (i32.add
                          (i32.sub
                            (local.get $l1)
                            (global.get $g0))
                          (i32.const 0)))
                      (i64.const -9223372036854775808)))
                  (if $I6
                    (i32.ge_u
                      (i32.add
                        (i32.add
                          (local.tee $l5
                            (i32.load align=1
                              (local.get $l4)))
                          (local.get $l1))
                        (i32.const 4))
                      (local.get $l0))
                    (then
                      (if $I7
                        (i32.eq
                          (memory.grow
                            (i32.add
                              (i32.div_u
                                (i32.sub
                                  (i32.add
                                    (i32.add
                                      (local.get $l5)
                                      (local.get $l1))
                                    (i32.const 4))
                                  (local.get $l0))
                                (i32.const 65536))
                              (i32.const 1)))
                          (i32.const -1))
                        (then
                          (unreachable))
                        (else
                          (local.set $l0
                            (i32.mul
                              (i32.const 65536)
                              (memory.size)))))))
                  (memory.copy
                    (local.get $l1)
                    (local.get $l4)
                    (i32.add
                      (local.get $l5)
                      (i32.const 4)))
                  (local.set $l1
                    (i32.add
                      (i32.add
                        (local.get $l5)
                        (i32.const 4))
                      (local.get $l1)))))))
          (local.set $l2
            (i32.add
              (local.get $l2)
              (i32.const 8)))
          (br $L2))))
    (memory.copy
      (i32.const 0)
      (global.get $g0)
      (i32.sub
        (local.get $l1)
        (global.get $g0)))
    (global.set $g0
      (i32.add
        (i32.sub
          (local.get $l1)
          (global.get $g0))
        (i32.const 0))))
  (func $f6 (type $t5) (param $p0 i32) (param $p1 i32)
    (local $l2 i32)
    (if $I0
      (i32.ge_u
        (local.get $p1)
        (i32.const 256))
      (then
        (unreachable)))
    (i32.store align=1
      (local.tee $l2
        (i32.mul
          (local.get $p1)
          (i32.const 8)))
      (local.get $p0))
    (i32.store offset=4 align=1
      (local.get $l2)
      (i32.const 0)))
  (func $f7 (type $t6) (param $p0 i64) (param $p1 i32)
    (local $l2 i32)
    (if $I0
      (i32.ge_u
        (local.get $p1)
        (i32.const 256))
      (then
        (unreachable)))
    (if $I1
      (i64.lt_u
        (local.get $p0)
        (i64.const -9223372036854775808))
      (then
        (i64.store align=1
          (i32.mul
            (local.get $p1)
            (i32.const 8))
          (local.get $p0)))
      (else
        (i32.store align=1
          (local.tee $l2
            (call $f4
              (i32.const 12)))
          (i32.const 8))
        (i64.store offset=4 align=1
          (local.get $l2)
          (local.get $p0))
        (i64.store align=1
          (i32.mul
            (local.get $p1)
            (i32.const 8))
          (i64.add
            (i64.extend_i32_u
              (local.get $l2))
            (i64.const -9223372036854775808))))))
  (func $f8 (type $t7) (param $p0 f32) (param $p1 i32)
    (local $l2 i32)
    (if $I0
      (i32.ge_u
        (local.get $p1)
        (i32.const 256))
      (then
        (unreachable)))
    (f32.store align=1
      (local.tee $l2
        (i32.mul
          (local.get $p1)
          (i32.const 8)))
      (local.get $p0))
    (i32.store offset=4 align=1
      (local.get $l2)
      (i32.const 0)))
  (func $f9 (type $t8) (param $p0 f64) (param $p1 i32)
    (local $l2 i32)
    (if $I0
      (i32.ge_u
        (local.get $p1)
        (i32.const 256))
      (then
        (unreachable)))
    (if $I1
      (i64.lt_u
        (i64.reinterpret_f64
          (local.get $p0))
        (i64.const -9223372036854775808))
      (then
        (f64.store align=1
          (i32.mul
            (local.get $p1)
            (i32.const 8))
          (local.get $p0)))
      (else
        (i32.store align=1
          (local.tee $l2
            (call $f4
              (i32.const 12)))
          (i32.const 8))
        (f64.store offset=4 align=1
          (local.get $l2)
          (local.get $p0))
        (i64.store align=1
          (i32.mul
            (local.get $p1)
            (i32.const 8))
          (i64.add
            (i64.extend_i32_u
              (local.get $l2))
            (i64.const -9223372036854775808))))))
  (func $f10 (type $t9) (param $p0 v128) (param $p1 i32)
    (local $l2 i32)
    (if $I0
      (i32.ge_u
        (local.get $p1)
        (i32.const 256))
      (then
        (unreachable)))
    (i32.store align=1
      (local.tee $l2
        (call $f4
          (i32.const 20)))
      (i32.const 16))
    (v128.store offset=4 align=1
      (local.get $l2)
      (local.get $p0))
    (i64.store align=1
      (i32.mul
        (local.get $p1)
        (i32.const 8))
      (i64.add
        (i64.extend_i32_u
          (local.get $l2))
        (i64.const -9223372036854775808))))
  (func $f11 (type $t3) (param $p0 i32) (result i32)
    (if $I0
      (i32.ge_u
        (local.get $p0)
        (i32.const 256))
      (then
        (unreachable)))
    (i32.load align=1
      (i32.mul
        (local.get $p0)
        (i32.const 8))))
  (func $f12 (type $t10) (param $p0 i32) (result i64)
    (local $l1 i64) (local $l2 i32)
    (if $I0
      (i32.ge_u
        (local.get $p0)
        (i32.const 256))
      (then
        (unreachable)))
    (if $I1 (result i64)
      (i64.lt_u
        (local.tee $l1
          (i64.load align=1
            (i32.mul
              (local.get $p0)
              (i32.const 8))))
        (i64.const -9223372036854775808))
      (then
        (local.get $l1))
      (else
        (if $I2
          (i32.eqz
            (local.tee $l2
              (i32.wrap_i64
                (i64.sub
                  (local.get $l1)
                  (i64.const -9223372036854775808)))))
          (then
            (unreachable)))
        (if $I3
          (i32.ne
            (i32.load align=1
              (local.get $l2))
            (i32.const 8))
          (then
            (unreachable)))
        (i64.load offset=4 align=1
          (local.get $l2)))))
  (func $f13 (type $t11) (param $p0 i32) (result f32)
    (if $I0
      (i32.ge_u
        (local.get $p0)
        (i32.const 256))
      (then
        (unreachable)))
    (f32.load align=1
      (i32.mul
        (local.get $p0)
        (i32.const 8))))
  (func $f14 (type $t12) (param $p0 i32) (result f64)
    (local $l1 i64) (local $l2 i32)
    (if $I0
      (i32.ge_u
        (local.get $p0)
        (i32.const 256))
      (then
        (unreachable)))
    (if $I1 (result f64)
      (i64.lt_u
        (local.tee $l1
          (i64.load align=1
            (i32.mul
              (local.get $p0)
              (i32.const 8))))
        (i64.const -9223372036854775808))
      (then
        (f64.reinterpret_i64
          (local.get $l1)))
      (else
        (if $I2
          (i32.eqz
            (local.tee $l2
              (i32.wrap_i64
                (i64.sub
                  (local.get $l1)
                  (i64.const -9223372036854775808)))))
          (then
            (unreachable)))
        (if $I3
          (i32.ne
            (i32.load align=1
              (local.get $l2))
            (i32.const 8))
          (then
            (unreachable)))
        (f64.load offset=4 align=1
          (local.get $l2)))))
  (func $f15 (type $t13) (param $p0 i32) (result v128)
    (local $l1 i64) (local $l2 i32)
    (if $I0
      (i32.ge_u
        (local.get $p0)
        (i32.const 256))
      (then
        (unreachable)))
    (if $I1
      (i64.lt_u
        (local.tee $l1
          (i64.load align=1
            (i32.mul
              (local.get $p0)
              (i32.const 8))))
        (i64.const -9223372036854775808))
      (then
        (unreachable)))
    (if $I2
      (i32.eqz
        (local.tee $l2
          (i32.wrap_i64
            (i64.sub
              (local.get $l1)
              (i64.const -9223372036854775808)))))
      (then
        (unreachable)))
    (if $I3
      (i32.ne
        (i32.load align=1
          (local.get $l2))
        (i32.const 16))
      (then
        (unreachable)))
    (v128.load offset=4 align=1
      (local.get $l2)))
  (func $f16 (type $t1) (param $p0 i32) (param $p1 i32) (param $p2 i32)
    (local $l3 i32)
    (if $I0
      (i32.ge_u
        (local.get $p2)
        (i32.const 256))
      (then
        (unreachable)))
    (if $I1
      (i32.eqz
        (local.get $p1))
      (then
        (i64.store align=1
          (i32.mul
            (local.get $p2)
            (i32.const 8))
          (i64.const -1)))
      (else
        (i32.store align=1
          (local.tee $l3
            (call $f4
              (i32.add
                (i32.const 4)
                (local.get $p1))))
          (local.get $p1))
        (call $memio.read
          (local.get $p0)
          (i32.add
            (local.get $l3)
            (i32.const 4))
          (local.get $p1))
        (i64.store align=1
          (i32.mul
            (local.get $p2)
            (i32.const 8))
          (i64.add
            (i64.extend_i32_u
              (local.get $l3))
            (i64.const -9223372036854775808))))))
  (func $f17 (type $t14) (param $p0 i32) (param $p1 i32) (param $p2 i32) (param $p3 i32)
    (local $l4 i32) (local $l5 i64) (local $l6 i32)
    (if $I0
      (i32.ge_u
        (local.get $p3)
        (i32.const 256))
      (then
        (unreachable)))
    (if $I1
      (i64.le_u
        (local.tee $l5
          (i64.load align=1
            (i32.mul
              (local.get $p3)
              (i32.const 8))))
        (i64.const -9223372036854775808))
      (then
        (unreachable)))
    (if $I2
      (i32.eq
        (local.tee $l4
          (i32.wrap_i64
            (i64.sub
              (local.get $l5)
              (i64.const -9223372036854775808))))
        (i32.const -1))
      (then)
      (else
        (local.set $l6
          (i32.load align=1
            (local.get $l4)))
        (if $I3
          (i32.gt_u
            (i32.add
              (local.get $p1)
              (local.get $p2))
            (local.get $l6))
          (then
            (unreachable)))
        (call $memio.write
          (i32.add
            (i32.add
              (local.get $l4)
              (i32.const 4))
            (local.get $p1))
          (local.get $p0)
          (local.get $p2)))))
  (func $f18 (type $t3) (param $p0 i32) (result i32)
    (local $l1 i64) (local $l2 i32)
    (if $I0
      (i32.ge_u
        (local.get $p0)
        (i32.const 256))
      (then
        (unreachable)))
    (if $I1
      (i64.le_u
        (local.tee $l1
          (i64.load align=1
            (i32.mul
              (local.get $p0)
              (i32.const 8))))
        (i64.const -9223372036854775808))
      (then
        (unreachable)))
    (if $I2 (result i32)
      (i32.eq
        (local.tee $l2
          (i32.wrap_i64
            (i64.sub
              (local.get $l1)
              (i64.const -9223372036854775808))))
        (i32.const -1))
      (then
        (i32.const 0))
      (else
        (i32.load align=1
          (local.get $l2)))))
  (func $f19 (type $t15) (param $p0 i32) (param $p1 i32) (param $p2 i32) (result i32)
    (local $l3 i64) (local $l4 i32) (local $l5 i32)
    (if $I0
      (i32.ge_u
        (local.get $p2)
        (i32.const 256))
      (then
        (unreachable)))
    (if $I1 (result i32)
      (i32.eqz
        (local.get $p1))
      (then
        (i32.const 0))
      (else
        (if $I2
          (i32.gt_u
            (local.get $p1)
            (i32.const 4))
          (then
            (unreachable)))
        (if $I3
          (i64.le_u
            (local.tee $l3
              (i64.load align=1
                (i32.mul
                  (local.get $p2)
                  (i32.const 8))))
            (i64.const -9223372036854775808))
          (then
            (unreachable)))
        (if $I4
          (i32.eq
            (local.tee $l4
              (i32.wrap_i64
                (i64.sub
                  (local.get $l3)
                  (i64.const -9223372036854775808))))
            (i32.const -1))
          (then
            (unreachable)))
        (local.set $l5
          (i32.load align=1
            (local.get $l4)))
        (if $I5
          (i32.gt_u
            (i32.add
              (local.get $p0)
              (local.get $p1))
            (local.get $l5))
          (then
            (unreachable)))
        (if $I6 (result i32)
          (i32.eq
            (local.get $p1)
            (i32.const 1))
          (then
            (i32.load8_u offset=4
              (i32.add
                (local.get $l4)
                (local.get $p0))))
          (else
            (if $I7 (result i32)
              (i32.eq
                (local.get $p1)
                (i32.const 2))
              (then
                (i32.load16_u offset=4 align=1
                  (i32.add
                    (local.get $l4)
                    (local.get $p0))))
              (else
                (if $I8 (result i32)
                  (i32.eq
                    (local.get $p1)
                    (i32.const 3))
                  (then
                    (i32.add
                      (i32.load16_u offset=4 align=1
                        (i32.add
                          (local.get $l4)
                          (local.get $p0)))
                      (i32.shl
                        (i32.load8_u offset=20
                          (i32.add
                            (local.get $l4)
                            (local.get $p0)))
                        (i32.const 16))))
                  (else
                    (i32.load offset=4 align=1
                      (i32.add
                        (local.get $l4)
                        (local.get $p0))))))))))))
  (func $f20 (type $t16) (param $p0 i32) (param $p1 i32) (param $p2 i32) (result i64)
    (local $l3 i64) (local $l4 i32) (local $l5 i32) (local $l6 i64) (local $l7 i32)
    (if $I0
      (i32.ge_u
        (local.get $p2)
        (i32.const 256))
      (then
        (unreachable)))
    (if $I1 (result i64)
      (i32.eqz
        (local.get $p1))
      (then
        (i64.const 0))
      (else
        (if $I2
          (i32.gt_u
            (local.get $p1)
            (i32.const 8))
          (then
            (unreachable)))
        (if $I3
          (i64.le_u
            (local.tee $l3
              (i64.load align=1
                (i32.mul
                  (local.get $p2)
                  (i32.const 8))))
            (i64.const -9223372036854775808))
          (then
            (unreachable)))
        (if $I4
          (i32.eq
            (local.tee $l4
              (i32.wrap_i64
                (i64.sub
                  (local.get $l3)
                  (i64.const -9223372036854775808))))
            (i32.const -1))
          (then
            (unreachable)))
        (local.set $l5
          (i32.load align=1
            (local.get $l4)))
        (if $I5
          (i32.gt_u
            (i32.add
              (local.get $p0)
              (local.get $p1))
            (local.get $l5))
          (then
            (unreachable)))
        (loop $L6 (result i64)
          (if $I7 (result i64)
            (i32.le_u
              (local.get $p1)
              (i32.shr_u
                (local.get $l7)
                (i32.const 3)))
            (then
              (local.get $l6))
            (else
              (local.set $l6
                (i64.add
                  (i64.shl
                    (i64.load8_u offset=4
                      (i32.add
                        (local.get $l4)
                        (local.get $p0)))
                    (i64.extend_i32_u
                      (local.get $l7)))
                  (local.get $l6)))
              (local.set $l7
                (i32.add
                  (i32.const 8)
                  (local.get $l7)))
              (br $L6)))))))
  (func $f21 (type $t17) (param $p0 i32) (param $p1 i32) (result f32)
    (local $l2 i64) (local $l3 i32) (local $l4 i32)
    (if $I0
      (i32.ge_u
        (local.get $p1)
        (i32.const 256))
      (then
        (unreachable)))
    (if $I1
      (i64.le_u
        (local.tee $l2
          (i64.load align=1
            (i32.mul
              (local.get $p1)
              (i32.const 8))))
        (i64.const -9223372036854775808))
      (then
        (unreachable)))
    (if $I2
      (i32.eq
        (local.tee $l3
          (i32.wrap_i64
            (i64.sub
              (local.get $l2)
              (i64.const -9223372036854775808))))
        (i32.const -1))
      (then
        (unreachable)))
    (local.set $l4
      (i32.load align=1
        (local.get $l3)))
    (if $I3
      (i32.gt_u
        (i32.add
          (local.get $p0)
          (i32.const 4))
        (local.get $l4))
      (then
        (unreachable)))
    (f32.load offset=4 align=1
      (i32.add
        (local.get $l3)
        (local.get $p0))))
  (func $f22 (type $t18) (param $p0 i32) (param $p1 i32) (result f64)
    (local $l2 i64) (local $l3 i32) (local $l4 i32)
    (if $I0
      (i32.ge_u
        (local.get $p1)
        (i32.const 256))
      (then
        (unreachable)))
    (if $I1
      (i64.le_u
        (local.tee $l2
          (i64.load align=1
            (i32.mul
              (local.get $p1)
              (i32.const 8))))
        (i64.const -9223372036854775808))
      (then
        (unreachable)))
    (if $I2
      (i32.eq
        (local.tee $l3
          (i32.wrap_i64
            (i64.sub
              (local.get $l2)
              (i64.const -9223372036854775808))))
        (i32.const -1))
      (then
        (unreachable)))
    (local.set $l4
      (i32.load align=1
        (local.get $l3)))
    (if $I3
      (i32.gt_u
        (i32.add
          (local.get $p0)
          (i32.const 8))
        (local.get $l4))
      (then
        (unreachable)))
    (f64.load offset=4 align=1
      (i32.add
        (local.get $l3)
        (local.get $p0))))
  (func $set-octs-ext (export "set-octs-ext") (type $t19) (param $p0 i32) (param $p1 i32) (result i32)
    (local $l2 i32)
    (if $I0 (result i32)
      (local.get $p0)
      (then
        (i32.store align=1
          (local.tee $l2
            (call $f4
              (i32.add
                (i32.const 4)
                (local.get $p0))))
          (local.get $p0))
        (i64.store align=1
          (i32.mul
            (local.get $p1)
            (i32.const 8))
          (i64.add
            (i64.extend_i32_u
              (local.get $l2))
            (i64.const -9223372036854775808)))
        (i32.add
          (local.get $l2)
          (i32.const 4)))
      (else
        (i64.store align=1
          (i32.mul
            (local.get $p1)
            (i32.const 8))
          (i64.const -1))
        (i32.const -1))))
  (func $get-space-ptr (export "get-space-ptr") (type $t3) (param $p0 i32) (result i32)
    (local $l1 i64)
    (if $I0
      (i32.ge_u
        (local.get $p0)
        (i32.const 256))
      (then
        (unreachable)))
    (if $I1
      (i64.le_u
        (local.tee $l1
          (i64.load align=1
            (i32.mul
              (local.get $p0)
              (i32.const 8))))
        (i64.const -9223372036854775808))
      (then
        (unreachable)))
    (i32.wrap_i64
      (i64.sub
        (local.get $l1)
        (i64.const -9223372036854775808))))
  (func $clear-space (export "clear-space") (type $t4)
    (memory.init $d0
      (i32.const 0)
      (i32.const 0)
      (i32.const 2048))
    (global.set $g0
      (i32.const 2048))
    (global.set $space-start
      (i32.const 0))
    (global.set $g2
      (i32.const 0)))
  (func $f26 (type $t0) (result i32)
    (block $B0
      (global.set $g2
        (if $I1 (result i32)
          (i32.ge_u
            (i32.const 0)
            (global.get $g2))
          (then
            (i32.add
              (i32.const 0)
              (i32.const 1)))
          (else
            (global.get $g2))))
      (call $f6
        (i32.const 0)
        (i32.const 0)))
    (call $f6
      (call $serf.writeHi)
      (i32.add
        (i32.const 0)
        (global.get $space-start)))
    (call $f11
      (i32.add
        (i32.const 0)
        (global.get $space-start))))
  (func $console/log (export "console/log") (type $t19) (param $p0 i32) (param $p1 i32) (result i32)
    (local $l2 i32) (local $l3 i32)
    (local.set $l2
      (global.get $space-start))
    (global.set $space-start
      (global.get $g2))
    (local.get $p1)
    (drop
      (i32.const 1))
    (drop
      (local.get $p1))
    (i32.add
      (i32.const 1)
      (global.get $space-start))
    (call $f6)
    (local.get $p0)
    (drop
      (i32.const 0))
    (drop
      (local.get $p1))
    (i32.add
      (i32.const 0)
      (global.get $space-start))
    (call $f6)
    (global.set $g2
      (if $I0 (result i32)
        (i32.ge_u
          (local.tee $l3
            (i32.add
              (i32.const 2)
              (global.get $space-start)))
          (global.get $g2))
        (then
          (i32.add
            (local.get $l3)
            (i32.const 1)))
        (else
          (global.get $g2))))
    (call $f6
      (i32.const 0)
      (local.get $l3))
    (global.set $g2
      (if $I1 (result i32)
        (i32.ge_u
          (local.tee $l3
            (i32.add
              (i32.const 3)
              (global.get $space-start)))
          (global.get $g2))
        (then
          (i32.add
            (local.get $l3)
            (i32.const 1)))
        (else
          (global.get $g2))))
    (if $I2
      (i32.ge_u
        (local.get $l3)
        (i32.const 256))
      (then
        (unreachable)))
    (i64.store align=1
      (i32.mul
        (local.get $l3)
        (i32.const 8))
      (i64.const -1))
    (call $f16
      (call $f11
        (i32.add
          (i32.const 0)
          (global.get $space-start)))
      (call $f11
        (i32.add
          (i32.const 1)
          (global.get $space-start)))
      (i32.add
        (i32.const 3)
        (global.get $space-start)))
    (i32.add
      (i32.const 3)
      (global.get $space-start))
    (global.set $space-clue
      (i32.const 1))
    (call $lia.printf)
    (call $f11
      (i32.add
        (i32.const 2)
        (global.get $space-start)))
    (global.set $space-start
      (local.get $l2)))
  (memory $M0 1)
  (global $g0 (mut i32) (i32.const 2048))
  (global $space-start (export "space-start") (mut i32) (i32.const 0))
  (global $g2 (mut i32) (i32.const 0))
  (global $space-clue (export "space-clue") (mut i32) (i32.const 0))
  (global $act-0-func-idx (export "act-0-func-idx") i32 (i32.const 26))
  (global $n-funcs (export "n-funcs") i32 (i32.const 1))
  (data $d0 (i32.const 0) "\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80\00\00\00\00\00\00\00\80")
  (data $d1 "\02"))
