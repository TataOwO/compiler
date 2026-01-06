class Main {
    public static void main(String args[]) {
	int x = 0;
        int y = 0;
	x = (x = 1 + (y = 2 + 3) + 4) + 5;;
        System.out.print(x + "\n");
        System.out.print(y + "\n");
    }
}
