        LDA #0
        LDX #10
loop:   STX $00
        ADC $00
        DEX
        BNE *-5 ; Should be the same as BNE loop
        BRK