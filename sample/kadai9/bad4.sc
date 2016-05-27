int a[10];
int main(){
	int i,sum,*point;
	int *b[10];
	sum = 0;
	for(i = 0; i < 10; i = i + 1){
		a[i] = i;
		point = a;
		b[i] = &a[i];
		sum = sum + a[i];
	}
}