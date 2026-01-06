class A {
    A() {}
    String f() { return "ok\n"; }
}
class Main {
    public static void main(String args[]) {
        A a = new A();
	for(System.out.print(a.f()) ; ; ) return;
    }
}
