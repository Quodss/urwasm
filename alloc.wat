;;  alloc and gc implementations in .wat for type checks
;;  will be obsolete with Wasm validator in Hoon
;;
(module
  (import "memio" "read"(func $memread (param i32 i32 i32)))
  (import "memio" "write" (func $memwrite (param i32 i32 i32)))
  (memory 1)
  (global $edge (mut i32) i32.const 0)
;;
  (func $alloc (param $len i32) (result i32)
    (local $edge_new i32) (local $memsize i32)
    local.get $len
    global.get 0
    i32.add
    local.tee $edge_new
    i32.const 65536 ;;  page_size
    memory.size
    i32.mul
    local.tee $memsize
    i32.ge_u
  ;;
    local.get $edge_new
    i32.const 8_388_608 ;;  heap_lim
    i32.ge_u
  ;;
    i32.or
    if
      call $gc
      local.get $len
      global.get 0
      i32.add
      local.tee $edge_new
      i32.const 8_388_608 ;;  heap_lim
      i32.ge_u
      if
        unreachable
      end
      local.get $edge_new
      local.get $memsize
      i32.ge_u
      if
        local.get $edge_new
        local.get $memsize
        i32.sub
        i32.const 65536 ;;  page_size
        i32.div_u
        i32.const 1
        i32.add
        memory.grow
        i32.const -1
        i32.eq
        if
          unreachable
        end
      end
    end
    global.get 0
    local.get $edge_new
    global.set 0
  )
  (func $gc
    (local $memsize i32) (local $edge_copy i32)
    (local $space_val_ptr i32) (local $space_val i32)
    (local $datum_size i32)
      i32.const 1_024 ;; space_size
      global.get 0
      i32.add
      local.tee $edge_copy
      i32.const 65_536 ;; page_size
      memory.size
      i32.mul
      local.tee $memsize
      i32.ge_u
      if  ;;  if edge_copy >= memsize: attempt to grow
        local.get $edge_copy
        local.get $memsize
        i32.sub
        i32.const 65_536 ;; page_size
        i32.div_u
        i32.const 1
        i32.add
        memory.grow
        i32.const -1
        i32.eq
        if  ;;  if memory_grow yields -1: crash, else update memsize
          unreachable
        else
          i32.const 65_536 ;; page_size
          memory.size
          i32.mul
          local.set $memsize
        end
      end
    ;;
      i32.const 0
      local.set $space_val_ptr
      loop
        local.get $space_val_ptr
        i32.const 1_024 ;; space_size
        i32.ge_u
        if  ;;  if space ptr address >= space_size: end loop
        else
          local.get $space_val_ptr
          i32.load offset=0 ;; o=offset
          local.tee $space_val
          i32.const 0x8000_0000 ;; null_ptr
          i32.lt_u
          if  ;;  if MSB set to 0 then it's numerical value, copy
            global.get 0
            local.get $space_val_ptr
            i32.add
            local.get $space_val
            i32.store
          else  ;;  else it's a pointer
            local.get $space_val
            i32.const 0x8000_0000 ;; null_ptr
            i32.sub
            local.tee $space_val  ;;  remove leading 1
            i32.eqz
            if  ;;  if NULL: write NULL to destination
              global.get 0
              local.get $space_val_ptr
              i32.add
              i32.const 0x8000_0000 ;; null_ptr
              i32.store
            else  ;;  else: set pointer, copy data
              global.get 0
              local.get $space_val_ptr
              i32.add
            ;;
              local.get $edge_copy
              global.get 0
              i32.sub
              i32.const 0 ;; offset
              i32.add             ;;  (copy_edge - edge + offset = edge after shift)
              i32.const 0x8000_0000 ;; null_ptr  ;;  leading 1
              i32.add
            ;;
              i32.store
            ;;
              local.get $space_val
              i32.load
              local.tee $datum_size
              local.get $edge_copy
              i32.add
              i32.const 4 ;; len_size
              i32.add
              local.get $memsize
              i32.ge_u
              if  ;;  if copy_edge+datum_size+len_size >= memsize: attempt to grow
                local.get $datum_size
                local.get $edge_copy
                i32.add
                i32.const 4 ;; len-size
                i32.add
                local.get $memsize
                i32.sub
                i32.const 65_536 ;; page_size
                i32.div_u
                i32.const 1
                i32.add
                memory.grow
                i32.const -1
                i32.eq
                if  ;;  if memory_grow yields -1: crash, else update memsize
                  unreachable
                else
                  i32.const 65_536 ;; page_size
                  memory.size
                  i32.mul
                  local.set $memsize
                end
              end
            ;;
              local.get $edge_copy  ;;  copy to edge_copy
              local.get $space_val  ;;  from ptr
              local.get $datum_size
              i32.const 4 ;; len_size
              i32.add     ;;  n+len_size bytes
              memory.copy
              local.get $datum_size
              i32.const 4 ;; len_size
              i32.add
              local.get $edge_copy
              i32.add
              local.set $edge_copy  ;;  update edge_copy
            end  ;;  end non-null pointer
          end    ;;  end pointer
        ;;
          local.get $space_val_ptr
          i32.const 4 ;; space_width
          i32.add
          local.set $space_val_ptr
          br 1  ;;  jump to loop
        end  ;;  end if
      end  ;;  end loop
    ;;
      i32.const 0 ;; offset  ;;  copy to offset
      global.get 0       ;;  from edge
      local.get $edge_copy
      global.get 0
      i32.sub           ;;  edge_copy - edge bytes
      memory.copy
    ;;
      local.get $edge_copy
      global.get 0
      i32.sub
      i32.const 0 ;; offset
      i32.add
      global.set 0  ;;  edge = edge_copy - edge + offset
  )
)
