#[cfg(test)]
mod tests {
    #[test]
    fn foo_bar_mod() {
        assert_eq!(2 + 2, 4);
    }

    #[no-test]
    fn not_a_test() {

    }
}
