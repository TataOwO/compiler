class Main {
    public static void main(String args[]) {
	int k = 0;
        int i = 0;
        int j;
	for(j = 10 ; i < j ; i=i+1) { k=k+1; j=j-1; }
	if (k == 5)
	    System.out.print("ok\n");
    }
}
