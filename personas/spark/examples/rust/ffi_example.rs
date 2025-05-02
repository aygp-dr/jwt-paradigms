// Rust code calling a C library
extern "C" {
    fn process_data(input: *const u8, len: usize) -> i32;
}

fn call_c_function(data: &[u8]) -> i32 {
    unsafe {
        process_data(data.as_ptr(), data.len())
    }
}
