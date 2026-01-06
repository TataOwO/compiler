class A {
    A() {}
    void m() {
        int i;
        for (i=0;     ;    ) return;
    }
}

class Main {
    public static void main(String arg[]) {
	new A().m();
	System.out.print("ok\n");
    }
}
