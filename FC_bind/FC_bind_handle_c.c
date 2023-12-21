//===========================================
//file: FC_bind_handle_c.c

#include <stdio.h>

void*  sometype_create(int len);
void*  sometype_free(void** h);
int*   sometype_a_ptr(void* h, int* n);
void*  sometype_x2(void* h);

int main(void) 
{
    // creates an instance of sometype with 6 elements in %a
    void* h = sometype_create( 6 );
    
    // get a pointer to %a and fill it. Alternatively one could 
    // define a "put" routine to avoid any access to %a from C
    int n;
    int* a = sometype_a_ptr(h, &n);
    for (int i = 0; i < n; i++) a[i] = i;
    
    // print the content of the instance
    for (int i = 0; i < n; i++) printf("%d  ",a[i]);
    printf("\n\n");
    
    // double all the values
    sometype_x2(h);
    
    // print the content of the instance
    for (int i = 0; i < n; i++) printf("%d  ",a[i]);
    printf("\n\n");

    // releases the instance
    sometype_free(&h);
    
    return 0;
}
//===========================================
