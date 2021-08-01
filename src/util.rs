
pub struct PrefixSuffix<'a, 'b> {
    pub prefix: &'a str,
    pub suffix: &'b str
}


impl PrefixSuffix<'_, '_> {

    pub fn format(&self, text: &str) -> String {
        format!("{}{}{}", self.prefix, text, self.suffix)
    }

}
