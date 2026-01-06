class Main {
    public static void main(String args[]) {
	int i = 0;
	int cpt = 0;
	for( ; i < 10; ) {
	    int j;
	    j = 10;
	    for ( ; j > 0; ) {
		cpt=cpt+1;
		j=j-1;
	    }
	    i=i+1;
	}
	if (cpt == 100)
	    System.out.print("ok\n");
    }
}
