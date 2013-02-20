default[:ghc] = {
  :version => "7.6.2",
  :arch    => kernel['machine'] =~ /x86_64/ ? "x86_64" : "i386"
}