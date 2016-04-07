int add(int a, int b){
  return a+b;
}

int tri(int a){
  int r = 0;
  for (int i = 1; i <= a; i++) {
    r = add(r, i);
  }
  return r;
}

int main(){
  return 0;
}

