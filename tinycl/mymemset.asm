    .686P
    .XMM
	.model	flat
_text SEGMENT
;PUBLIC _memset
    ;; memset(void* dst, int c, size_t count)
    ;;  [esp+0] RA
    ;;  [esp+4] dst
    ;;  [esp+8] c
    ;;  [esp+12] count
_memset PROC
    mov [esp-4], edi
    mov edi, [esp+8]

    mov edi, [esp+4]    ; edi = dst
    mov eax, [esp+8]    ; eax = c
    mov ecx, [esp+12]   ; ecx = count
    rep stosb
    ret
_memset ENDP
_text ENDS
END
