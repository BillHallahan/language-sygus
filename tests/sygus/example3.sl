(set-logic BV)
(synth-fun f ((x (_ BitVec 32))) (_ BitVec 32)
    ((BV32 (_ BitVec 32)) (BV16 (_ BitVec 16)))
    ((BV32 (_ BitVec 32) (#x00000000 #x00000001 #xFFFFFFFF
                          x
                          (bvand BV32 BV32)
                          (bvor BV32 BV32)
                          (bvnot BV32)
                          (concat BV16 BV16)
                          ))

     (BV16 (_ BitVec 16) (#x0000 #x0001 #xFFFF
                         (bvand BV16 BV16)
                         (bvor BV16 BV16)
                         (bvnot BV16)
                         ((_ extract 31 16) BV32)
                         ((_ extract 15 0) BV32)))))
(constraint (= (f #x0782ECAD) #xECAD0000))
(constraint (= (f #xFFFF008E) #x008E0000))
(constraint (= (f #x00000000) #x00000000))
(check-synth)