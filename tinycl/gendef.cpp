// Generate DEF file for Windows Linker
// @(#)$Id: //proj/evedit2/mainline/tinycl/gendef.cpp#4 $
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// warning C4996: 'fopen': This function or variable may be unsafe. Consider using fopen_s instead. To disable deprecation, use _CRT_SECURE_NO_WARNINGS. See online help for details.
#pragma warning(disable: 4996)


#define lengthof(v) ( sizeof(v) / sizeof(*(v)) )

inline bool isWordChar(char ch)
{
    if (ch >= 'A' && ch <=' Z') return true;
    if (ch >= 'a' && ch <= 'z') return true;
    if (ch >= '0' && ch <= '9') return true;
    return ch == '_';
} // isWordChar

class WordList
{
    public: struct WordItem
    {
        WordItem*   m_pNext;
        char*       m_psz;

        WordItem(char* psz) :
            m_pNext(NULL),
            m_psz(psz) {}
    }; // WordItem

    private: WordItem*  m_pFirst;
    private: WordItem*  m_pLast;
    private: char       m_sz[1024];

    public: WordList(char* psz) :
        m_pFirst(NULL),
        m_pLast(NULL)
    {
        psz = strcpy(m_sz, psz);
        char* pszWord = psz;
        while (0 != *psz)
        {
            if (isWordChar(*psz))
            {
                psz++;
            }
            else
            {
                *psz = 0;
                Append(new WordItem(pszWord));

                for (;;)
                {
                    psz++;
                    if (isWordChar(*psz)) break;
                    if (0 == *psz) break;
                } // word

                pszWord = psz;
            } // if
        } // while

        if (pszWord != psz)
        {
            Append(new WordItem(pszWord));
        }
    } // WordList

    public: WordItem* Append(WordItem* pWordItem)
    {
        if (NULL == m_pLast)
        {
            m_pFirst = pWordItem;
        }
        else
        {
            m_pLast->m_pNext = pWordItem;
        }

        return m_pLast = pWordItem;
    } // Append

    // [G]
    public: WordItem* GetFirst() const
        { return m_pFirst; }

    public: WordItem* GetLast() const
        { return m_pLast; }

    public: WordItem* GetNth(int k)
    {
        WordItem* pRunner = m_pFirst;
        while (k > 0)
        {
            if (NULL == pRunner) break;
            pRunner = pRunner->m_pNext;
            k -= 1;
        } // while
        return pRunner;
    } // GetNth
}; // WordList


void process(FILE* fp)
{
    char szLine[1024];
    while (NULL != fgets(szLine, sizeof(szLine), fp))
    {
        char* psz = szLine;

        while (' ' == *psz)
        {
            psz++;
        }

        switch (*psz)
        {
        case '#':
            continue;

        case '/':
            while ('/' == *psz)
            {
                *psz++ = ';';
            }
            break;

        default:
        {
            WordList oWordList(szLine);

            const char* pszFirst = oWordList.GetFirst()->m_psz;

            if (0 == strcmp(pszFirst, "CommandEntry"))
            {
                printf("Command_%s\n", oWordList.GetLast()->m_psz);
            }
            else if (0 == strcmp(pszFirst, "defmacro"))
            {
                printf("expand_%s\n", oWordList.GetLast()->m_psz);
            }
            else if (0 == strcmp(pszFirst, "FUN_BOOL"))
            {
                printf("%s\n", oWordList.GetNth(1)->m_psz);
            }
            else if (0 == strcmp(pszFirst, "FUN_ENTRY"))
            {
                printf("%s\n", oWordList.GetNth(6)->m_psz);
            }
            else if (0 == strcmp(pszFirst, "FUN_FIX"))
            {
                printf("%s\n", oWordList.GetNth(1)->m_psz);
            }
            else if (0 == strcmp(pszFirst, "FUN_FIX_SETF"))
            {
                printf("setf_%s\n", oWordList.GetNth(1)->m_psz);
            }
            else if (0 == strcmp(pszFirst, "FUN_REST"))
            {
                printf("%sV\n", oWordList.GetNth(1)->m_psz);
            }
            else if (0 == strcmp(pszFirst, "FUN_REST_SETF"))
            {
                printf("setf_%s\n", oWordList.GetNth(1)->m_psz);
            }
            else if (0 == strcmp(pszFirst, "FUN_VAR"))
            {
                printf("%s_\n", oWordList.GetNth(1)->m_psz);
            }
            else if (0 == strcmp(pszFirst, "FUN_VAR_SETF"))
            {
                printf("setf_%s_\n", oWordList.GetNth(1)->m_psz);
            }
            else if (0 == strcmp(pszFirst, "METHOD_FIX"))
            {
                printf("%s__%s\n",
                    oWordList.GetNth(1)->m_psz,
                    oWordList.GetNth(2)->m_psz );
            }
            else if (0 == strcmp(pszFirst, "METHOD_FIX_SETF"))
            {
                printf("setf_%s__%s\n",
                    oWordList.GetNth(1)->m_psz,
                    oWordList.GetNth(2)->m_psz );
            }
            else if (0 == strcmp(pszFirst, "METHOD_REST"))
            {
                printf("%s__%s\n",
                    oWordList.GetNth(1)->m_psz,
                    oWordList.GetNth(2)->m_psz );
            }
            else if (0 == strcmp(pszFirst, "METHOD_VAR"))
            {
                printf("%s__%s_\n",
                    oWordList.GetNth(1)->m_psz,
                    oWordList.GetNth(2)->m_psz );
            }
            else
            {
                break;
            }
            continue;
        } // default
        } // switch ch

        fputs(szLine, stdout);
    } // while
} // process

int main(int, char* argv[])
{
    const char* pszProg = argv[0];
    for (;;)
    {
        ++argv;
        if (NULL == *argv) break;
        FILE* fp = fopen(*argv, "r");
        if (NULL == fp)
        {
            fprintf(stderr, "%s: Can't open %s\n", pszProg, *argv);
        }
        else
        {
            printf(";; %s\n", *argv);
            process(fp);
            fclose(fp);
        }
    } // for

    fputs("\n;;; EOF\n", stdout);
    return 0;
} // main
