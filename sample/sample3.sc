
int isprime[1000];

void calcprime(int arraynum){
	int i,j;
	int shou,amari;
	i = 0;
	j = 0;
	isprime[2] = 1;
	for(i = 3; i < arraynum; i = i + 1){
		shou = i / 2;
		amari = i - (shou * 2);
		if(amari == 0){
			isprime[i] = 0;
		}else{
			isprime[i] = 1;
		}
	}
	for(i = 2; (i * i) < arraynum; i = i + 1){
		for(j = i+1; j < arraynum; j = j + 2){
			shou = (j / i);
			amari = j - (i * shou);
			if(amari == 0) isprime[j] = 0;
		}
	}
}

int main(){
	int made,index;
	made = 1000;
	calcprime(made);

	index = 0;
	for(index = 2; index < made; index = index + 1){
		if(isprime[index] == 1) print(index);
	}
	return 0;
}