// micro-C example 4 -- compute and print array of factorials

int a[20];			/* Must be global */

void main(int n) { 
  int i; 
  i = 0; 
  int f; 
  f = 1;
  while (i < n) {
    a[i] = f;
    i = i + 1;
    f = f + f;
  }
  int res;
  arrsum(n, a, &res);
  print res;
}


void arrsum (int n, int arr[], int *sump){
  int i;
  i = 0;
  *sump = 0;
  while (i < n) {
    *sump = *sump + arr[i];
    i=i+1;
  }
  print *sump;
}
