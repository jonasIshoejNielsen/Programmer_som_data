// micro-C example 4 -- compute and print array of factorials

int a[20];			/* Must be global */


void main(int n) { 
  int i; 
  i = 0; 
  int f; 
  f = 0;
  int max;
  max = 7;
  while (i < n) {
    a[i] = f;
    i = i + 1;
    f = f+i;
    if (f>=max)
      f=0;  
  }
  int freq[7];
  histogram (n, a, max, freq);
  i = 0;
  while (i < max) {
    print freq[i];
    i=i+1;
  }
}

void histogram(int n, int ns[], int max, int freq[]){
  int x;
  x = 0;
  while (x < max) {
    freq[x] = 0;
    x=x+1;
  }
  int i;
  i = 0;
  while (i < n) {
    freq[ns[i]] = freq[ns[i]] + 1;
    i=i+1;
  }
}

