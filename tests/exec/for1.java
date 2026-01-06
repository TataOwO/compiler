class Main {
    public static void main(String args[]) {
	int i;
	int cpt = 0;
	for(i = 0; i < 10; i=i+1) {
            int j;
	    for (j = 10; j > 0; j=j-1)
		cpt = cpt+1;
        }
	if (cpt == 100)
	    System.out.print("ok\n");
    }
}
