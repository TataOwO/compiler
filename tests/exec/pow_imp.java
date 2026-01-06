class Puiss {
    Puiss() {}
    int puis(int a, int n) {
	int r = 1;
        int i;
	for(i = 0; i < n / 2; i=i+1)
	    r = r * a;
	r = r * r;
	if (n % 2 != 0)
	    r = r * a;
	return r;
    }
}

class Main {
    public static void main(String args[]) {
        Puiss p = new Puiss();
	if (p.puis(2, 8) == 256 && p.puis(6, 3) == 216)
	    System.out.print("ok\n");
    }
}
