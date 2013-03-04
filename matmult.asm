;
; ***************************************************************
;       SKELETON: INTEL ASSEMBLER MATRIX MULTIPLY (LINUX)
; ***************************************************************
;
;
; --------------------------------------------------------------------------
; class matrix {
;     int ROWS              // ints are 64-bit
;     int COLS
;     int elem [ROWS][COLS]
;
;     void print () {
;         output.newline ()
;         for (int row=0; row < this.ROWS; row++) {
;             for (int col=0; col < this.COLS; cols++) {
;                 output.tab ()
;                 output.int (this.elem[row, col])
;             }
;             output.newline ()
;         }
;     }
;
;     void mult (matrix A, matrix B) {
;         for (int row=0; row < this.ROWS; row++) {
;             for (int col=0; col < this.COLS; cols++) {
;                 int sum = 0
;                 for (int k=0; k < A.COLS; k++)
;                     sum = sum + A.elem[row, k] * B.elem[k, col]
;                 this.elem [row, col] = sum
;             }
;         }
;     }
; }
; ---------------------------------------------------------------------------
; main () {
;     matrix matrixA, matrixB, matrixC  ; Declare and suitably initialise
;                                         matrices A, B and C
;     matrixA.print ()
;     matrixB.print ()
;     matrixC.mult (matrixA, matrixB)
;     matrixC.print ()
; }
; ---------------------------------------------------------------------------
;
; Notes:
; 1. For conditional jump instructions use the form 'Jxx NEAR label'  if label
;    is more than 127 bytes from the jump instruction Jxx.
;    For example use 'JGE NEAR end_for' instead of 'JGE end_for', if the
;    assembler complains that the label end_for is more than 127 bytes from
;    the JGE instruction with a message like 'short jump is out of  range'
;
;
; ---------------------------------------------------------------------------

segment .text
        global  _start
_start:

main:
          mov  rax, matrixA     ; matrixA.print ()
          push rax
          call matrix_print
          add  rsp, 8

          mov  rax, matrixB     ; matrixB.print ()
          push rax
          call matrix_print
          add  rsp, 8

          mov  rax, matrixB     ; matrixC.mult (matrixA, matrixB)
          push rax
          mov  rax, matrixA
          push rax
          mov  rax, matrixC
          push rax
          call matrix_mult
          add  rsp, 24          ; pop parameters & object reference

          mov  rax, matrixC     ; matrixC.print ()
          push rax
          call matrix_print
          add  rsp, 8

          call os_return                ; return to operating system

; ---------------------------------------------------------------------

matrix_print:                   ; void matrix_print ()
         push rbp                ; setup base pointer
         mov  rbp, rsp

         ;
         ; *********************************************
         
         push r8                ; push current value of r8
         push r9                ; push current value of r9
         push r10               ; push current value of r10
         push r11               ; push current value of r11
         push r12               ; push current value of r12
         push r13               ; push current value of r13
         push r14               ; push current value of r14
         
         call output_newline    ; outputs a new line
         
         mov r8, [rbp+16]       ; r8 = address of matrix
         mov r9, [r8]           ; r9 = ROWS
         mov r10, [r8+8]        ; r10 = COLS
         add r8, 16             ; sets r8 to the starting position of the
                                ; matrix values, ie. jumps over rows/cols
         
         forRow:
           mov r11, 0           ; r11 = row = 0
         
         nextRow:
           cmp r11, r9          ; compare row and ROWS
           jge endForRow        ; if row >= ROWS, jump
           
           ; otherwise:
           
           forCol:
             mov r12, 0         ; r12 = col = 0
             
           nextCol:
             cmp r12, r10       ; compare col and COLS
             jge endForCol      ; if col >= COLS, jump
             
             ; otherwise:
             
             call output_tab    ; outputs a new tab
             
             ; Position of required element:
             ;   start + 8 * (row * COLS + col)
             mov r14, r11       ; r14 = r11 = row
             imul r14, r10      ; r14 = row * COLS
             add r14, r12       ; r14 = row * COLS + col
             imul r14, 8        ; r14 = 8 * (row * COLS + col)
             add r14, r8        ; r14 = start + 8 * (row * COLS + col)
             
             mov r13, [r14]     ; move correct matrix element into r13
             
             push r13
             call output_int    ; outputs the integer at the position [row][col]
             add rsp, 8
             
             inc r12            ; col++
             jmp nextCol        ; next col iteration
             
           endForCol:
             call output_newline ; outputs a new line
             inc r11             ; row++
             jmp nextRow         ; next row iteration
           
         endForRow:
           ; end here
           
         pop r14                ; restore original value of r14
         pop r13                ; restore original value of r13
         pop r12                ; restore original value of r12
         pop r11                ; restore original value of r11
         pop r10                ; restore original value of r10
         pop r9                 ; restore original value of r9
         pop r8                 ; restore original value of r8

         ; *********************************************
         ;

         pop  rbp                ; restore base pointer & return
         ret

;  --------------------------------------------------------------------------

matrix_mult:                    ; void matix_mult (matrix A, matrix B)

         push rbp                ; setup base pointer
         mov  rbp, rsp

         ;
         ; *********************************************
         
         push r8                     ; push current value of r8
         push r9                     ; push current value of r9
         push r10                    ; push current value of r10
         push r11                    ; push current value of r11
         push r12                    ; push current value of r12
         push r13                    ; push current value of r13
         push r14                    ; push current value of r14
         push r15                    ; push current value of r15

         mov r8, [rbp+16]            ; r8 = address of matrix C
         mov r9, [rbp+24]            ; r9 = address of matrix A
         mov r10, [rbp+32]           ; r10 = address of matrix B
         
         ; [r8] = c.ROWS
         ; [r8+8] = c.COLS         
         
         forMultRows:
           mov r11, 0                ; r11 = row = 0
         
         nextMultRow:
           cmp r11, [r8]             ; compare row and C.ROWS
           jge endForMultRows        ; if row >= C.ROWS, jump
           
           ; otherwise:
           
           forMultCols:
             mov r12, 0              ; r12 = col = 0
             
           nextMultCol:
             cmp r12, [r8+8]         ; compare col and C.COLS
             jge endForMultCols      ; if col >= C.COLS, jump
               
             ; otherwise:
                              
             forAMultCols:
               mov r13, 0            ; r13 = k = 0
               mov qword[rbp-8], 0   ; sum = [rbp-8] = 0
                 
             nextAMultCol:
               cmp r13, [r9+8]       ; compare k and A.COLS
               jge endForAMultCols   ; if k >= A.COLS, jump
                   
               ; otherwise:
               
               ; Position of required element:
               ;   start of A + 16 + 8 * (row * A.COLS + k)
               ;   Note: we add 16 to 'skip' the row and col attributes.
               
               mov r14, r11          ; r14 = row
               imul r14, [r9+8]      ; r14 = row * A.COLS
               add r14, r13          ; r14 = row * A.COLS + k
               imul r14, 8           ; r14 = 8 * (row * A.COLS + k)
               add r14, r9           ; r14 = start of A + 8 * (row * A.COLS + k)
               add r14, 16           ; r14 = 16 + start + 8 * (row * A.COLS + k)
               
               mov r15, [r14]        ; r15 = A.elem[row,k]
               
               ; Position of required element:
               ;   start of B + 16 + 8 * (k * B.COLS + col)
               ;   Note: we add 16 to 'skip' the row and col attributes.
               
               mov r14, r13          ; r14 = k
               imul r14, [r10+8]     ; r14 = k * B.COLS
               add r14, r12          ; r14 = k * B.COLS + col
               imul r14, 8           ; r14 = 8 * (k * B.COLS + col)
               add r14, r10          ; r14 = start of B + 8 * (k * B.COLS + col)
               add r14, 16           ; r14 = 16 + start + 8 * (k * B.COLS + col)
               
               imul r15, [r14]       ; r15 = A.elem[row,k] * B.elem[k, col]
               jo overflowError      ; Jumps to print an error upon overflow
               
               add [rbp-8], r15      ; sum = sum + A.elem[row,k] * B.elem[k,col]
                  
               inc r13               ; k++
               jmp nextAMultCol      ; next k loop iteration
                   
             endForAMultCols:
                   
               ; Position of required element:
               ;   start of C + 16 + 8 * (row * COLS + col)
               ;   Note: we add 16 to 'skip' the row and col attributes.
               
               mov r14, r11          ; r14 = row
               imul r14, [r8+8]      ; r14 = row * COLS
               add r14, r12          ; r14 = row * COLS + col
               imul r14, 8           ; r14 = 8 * (row * COLS + col)
               add r14, r8           ; r14 = start of C + 8 * (row * COLS + col)
               add r14, 16           ; r14 = 16 + start + 8 * (row * COLS + col)
                   
               mov r15, [rbp-8]      ; move the sum into r15
               mov qword[r14], r15   ; move r15 (sum) into the matrix position
               
               
             inc r12                 ; col++
             jmp nextMultCol         ; next col iteration
               
           endForMultCols:
             inc r11                 ; row++
             jmp nextMultRow         ; next row iteration
           
         overflowError:
           ; An overflow has occurred, let's print a helpful message!
           push 'O'
           call output_char
           add rsp, 8
           push 'v'
           call output_char
           add rsp, 8
           push 'e'
           call output_char
           add rsp, 8
           push 'r'
           call output_char
           add rsp, 8
           push 'f'
           call output_char
           add rsp, 8
           push 'l'
           call output_char
           add rsp, 8
           push 'o'
           call output_char
           add rsp, 8
           push 'w'
           call output_char
           add rsp, 8
           push '!'
           call output_char
           add rsp, 8
           
         endForMultRows:
           ; We're done!
           
         pop r15                     ; restore original value of r15
         pop r14                     ; restore original value of r14
         pop r13                     ; restore original value of r13
         pop r12                     ; restore original value of r12
         pop r11                     ; restore original value of r11
         pop r10                     ; restore original value of r10
         pop r9                      ; restore original value of r9
         pop r8                      ; restore original value of r8
         
         ; *********************************************
         ;

         pop  rbp                ; restore base pointer & return
         ret


; ---------------------------------------------------------------------
;                    ADDITIONAL METHODS

CR      equ     13              ; carriage-return
LF      equ     10              ; line-feed
TAB     equ     9               ; tab
MINUS   equ     '-'             ; minus

LINUX   equ     80H             ; interupt number for entering Linux kernel
EXIT    equ     1               ; Linux system call 1 i.e. exit ()
WRITE   equ     4               ; Linux system call 4 i.e. write ()
STDOUT  equ     1               ; File descriptor 1 i.e. standard output

; ------------------------

os_return:
        mov  rax, EXIT          ; Linux system call 1 i.e. exit ()
        mov  rbx, 0             ; Error code 0 i.e. no errors
        int  LINUX              ; Interrupt Linux kernel

output_char:                    ; void output_char (ch)
        push rax
        push rbx
        push rcx
        push rdx
        push r8                ; r8..r11 are altered by Linux kernel interrupt
        push r9
        push r10
        push r11
        push qword [octetbuffer] ; (just to make output_char() re-entrant...)

        mov  rax, WRITE         ; Linux system call 4; i.e. write ()
        mov  rbx, STDOUT        ; File descriptor 1 i.e. standard output
        mov  rcx, [rsp+80]      ; fetch char from non-I/O-accessible segment
        mov  [octetbuffer], rcx ; load into 1-octet buffer
        lea  rcx, [octetbuffer] ; Address of 1-octet buffer
        mov  rdx, 1             ; Output 1 character only
        int  LINUX              ; Interrupt Linux kernel

        pop qword [octetbuffer]
        pop  r11
        pop  r10
        pop  r9
        pop  r8
        pop  rdx
        pop  rcx
        pop  rbx
        pop  rax
        ret

; ------------------------

output_newline:                 ; void output_newline ()
       push qword LF
       call output_char
       add rsp, 8
       ret

; ------------------------

output_tab:                     ; void output_tab ()
       push qword TAB
       call output_char
       add  rsp, 8
       ret

; ------------------------

output_minus:                   ; void output_minus()
       push qword MINUS
       call output_char
       add  rsp, 8
       ret

; ------------------------

output_int:                     ; void output_int (int N)
       push rbp
       mov  rbp, rsp

       ; rax=N then N/10, rdx=N%10, rbx=10

       push rax                ; save registers
       push rbx
       push rdx

       cmp  qword [rbp+16], 0 ; minus sign for negative numbers
       jge  L88

       call output_minus
       neg  qword [rbp+16]

L88:
       mov  rax, [rbp+16]       ; rax = N
       mov  rdx, 0              ; rdx:rax = N (unsigned equivalent of "cqo")
       mov  rbx, 10
       idiv rbx                ; rax=N/10, rdx=N%10

       cmp  rax, 0              ; skip if N<10
       je   L99

       push rax                ; output.int (N / 10)
       call output_int
       add  rsp, 8

L99:
       add  rdx, '0'           ; output char for digit N % 10
       push rdx
       call output_char
       add  rsp, 8

       pop  rdx                ; restore registers
       pop  rbx
       pop  rax
       pop  rbp
       ret


; ---------------------------------------------------------------------

segment .data

        ; Declare test matrices
matrixA DQ 2                    ; ROWS
        DQ 3                    ; COLS
        DQ 1, 2, 3              ; 1st row
        DQ 4, 5, 6              ; 2nd row

matrixB DQ 3                    ; ROWS
        DQ 2                    ; COLS
        DQ 1, 2                 ; 1st row
        DQ 3, 4                 ; 2nd row
        DQ 5, 6                 ; 3rd row

matrixC DQ 2                    ; ROWS
        DQ 2                    ; COLS
        DQ 0, 0                 ; space for ROWS*COLS ints
        DQ 0, 0                 ; (for filling in with matrixA*matrixB)

; ---------------------------------------------------------------------

        ; The following is used by output_char - do not disturb
        ;
        ; space in I/O-accessible segment for 1-octet output buffer
octetbuffer     DQ 0            ; (qword as choice of size on stack)
