void swap(int *a,int *b){
	int c;
	c = *a;
	*a = *b;
	*b = c;
}

int main(){
	int a[10],b[10],i;
	int *c;
	for(i = 0; i < 10; i = i + 1){
		a[i] = i;
		b[i] = 10 - i;
	}
	for(i = 0; i < 10; i = i + 1){
		swap(&a[i],&b[i]);
	}
	i = 0;
	for(i = 0; i < 10; i = i + 1){
		if(a[i] == b[i] && (&a[i] == &b[i] || a == c)){
			print(a[i]);
		}
	}
}