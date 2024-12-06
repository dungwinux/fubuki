start:
    LDA #10
    LDX #0
    STA $10, X
    INX
    STA $10, X
    LSR A
    LDY $10, X
    JMP ($FFFE)