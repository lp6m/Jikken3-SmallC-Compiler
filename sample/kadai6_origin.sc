int main(){
	int *a,*b,c[10],d;
	int e,f,g,i;
	a = &(*b);
	d = c[4];
	e = (f = 4, g = 6);
	for(i = 0, d = 2; (i < 4 && d < 3); i = i + 1){
		print(i);
	}
	i = 4 * -4 * 6;
	for(; ; i = i + 1) print(i);
	for(;;);	
}