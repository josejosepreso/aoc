import java.io.*;
import java.util.*;
import java.util.stream.*;

class FileHandler {
		public static final Optional<String> readFile(String filename) throws FileNotFoundException {
				try (BufferedReader file = new BufferedReader(new FileReader(filename))) {
						return Optional.of(file.lines().collect(Collectors.joining("\n")));
				} catch (Exception e) {
						return Optional.empty();
				}
		}
}

class Solution {
		public static int partOne(String input) {
				return Arrays.stream(input.split("\n"))
						.map(String::trim)
						.map(Integer::parseInt)
						.reduce(0, (acc, n) -> acc + n);
		}

		public static int partTwo(String input) {
				int freq = 0;

				final Integer[] nums = Arrays.stream(input.split("\n"))
						.map(String::trim)
						.map(Integer::parseInt)
						.toArray(Integer[]::new);

				Integer[] seen = new Integer[nums.length];
				seen[0] = 0;

				for (int i = 0; ; ++i) {
						freq += nums[i % nums.length];

						if (ArrayUtils.elem(freq, seen)) {
								return freq;
						}

						if (i + 1 >= seen.length) {
								seen = ArrayUtils.copy(seen, new Integer[seen.length + nums.length]);
						}

						seen[i + 1] = freq;
				}
		}
}

class ArrayUtils {
		public static final <T> T[] tail (T[] xs) {
				T[] result = (T[]) new Object[xs.length - 1];

				for (int i = 1; i < xs.length; ++i) {
						result[i - 1] = xs[i];
				}

				return result;
		}

		public static final <T> boolean elem(T x, T[] xs) {
				if (xs.length == 0) {
						return false;
				}

				return x == xs[0] || ArrayUtils.elem(x, ArrayUtils.tail(xs));
		}

		public static final boolean elem(int x, int[] xs) {
				return ArrayUtils.elem((Integer) x, Arrays.stream(xs).boxed().toArray(Integer[]::new));
		}

		// copy xs to ys
		public static <T> T[] copy(T[] xs, T[] ys) {
				for (int i = 0; i < xs.length; ++i) {
						ys[i] = xs[i];
				}

				return ys;
		}

		public static <T> String toString(T[] xs) {
				StringBuilder result = new StringBuilder("[");

				final T[] nonNulls = (T[]) Arrays.stream(xs).filter(e -> e != null).toArray(Object[]::new);
				final int length = nonNulls.length;

				for (int i = 0; i < length; ++i) {
						result.append(String.format(" %s", nonNulls[i].toString()));

						if (i < length - 1) {
								result.append(",");
						}
				}

				result.append("]");

				return result.toString();
		}
}

class Main {
		public static void main(String[] args) throws FileNotFoundException {
				FileHandler.readFile("input").ifPresent(input -> {
								System.out.println(Solution.partOne(input));
								System.out.println(Solution.partTwo(input));
						});
		}
}
