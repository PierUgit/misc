//===========================================
//file: FC_bind_id_c.c

#include <stdio.h>

int    sometype_create(int len);
void*  sometype_free(int* id);
int*   sometype_a_ptr(int id, int* n);
void*  sometype_x2(int id);

int main(void) 
{
    // creates an instance of sometype with 6 elements in %a
    int id = sometype_create( 6 );
    
    // get a pointer to %a and fill it. Alternatively one could 
    // define a "put" routine to avoid any access to %a from C
    int n;
    int* a = sometype_a_ptr(id, &n);
    for (int i = 0; i < n; i++) {
        a[i] = i;
    }
    
    // print the content of the instance
    for (int i = 0; i < n; i++) printf("%d  ",a[i]);
    printf("\n\n");
    
    // double all the values
    sometype_x2(id);
    
    // print the content of the instance
    for (int i = 0; i < n; i++) printf("%d  ",a[i]);
    printf("\n\n");

    // releases the instance
    sometype_free(&id);
    
    return 0;
}
//===========================================
