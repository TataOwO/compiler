class Fact {
    Fact() {}
    int fact_rec(int n) {
	if (n <= 1) return 1;
	return n * fact_rec(n - 1);
    }
}
class Main {
    public static void main(String args[]) {
        Fact f = new Fact();
	if (f.fact_rec(5) == 120)
	    System.out.print("ok\n");
    }
}
