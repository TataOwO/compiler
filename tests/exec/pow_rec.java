class Puiss {
    Puiss() {}
    int puis(int a, int n) {
	if (n <= 0)
	    return 1;
	int r = puis(a, n / 2);
	r = r * r;
	if (n % 2 == 0)
	    return r;
	return r * a;
    }
}
class Main {
    public static void main(String args[]) {
        Puiss p = new Puiss();
	if (p.puis(2, 4) == 16 && p.puis(6, 3) == 216)
	    System.out.print("ok\n");
    }
}
