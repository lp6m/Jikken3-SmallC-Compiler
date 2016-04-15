
int sumvar(int a, int b){
	return a + b;
}

int main(){
	int a,b;
	a = 3, b = 4;
	print(sumvar(a,b));
	if(sumvar(a,b) == 7){
		print(sumvar(a,b));
	}
}