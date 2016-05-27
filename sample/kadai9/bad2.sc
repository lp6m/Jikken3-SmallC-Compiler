void swap(int *a,int *b);
int a,b;
int main(){
	a = 1; b = 2;
	swap(&a,&b);
	print(a);
}

void swap(void a,int *b){
	int c;
	c = *a;
	*a = *b;
	*b = c;
}