
cookbook_file "/etc/profile.d/cabal.sh" do
	owner "root"
	group "root"
	mode 0755
end

%w(libgmp3-dev).each do |pkg|
	package(pkg) do
		action :install
	end
end

case [node.platform, node.platform_version]
when ["ubuntu", "12.04"] then
	package "libgmp3c2"
	
	link "/usr/lib/libgmp.so.3" do
		to "/usr/lib/libgmp.so.3.5.2"
		not_if "test -L /usr/lib/libgmp.so.3"
	end
end

require "tmpdir"

td = Dir.tmpdir
local_ghc_tarball = File.join(td, "ghc-#{node.ghc.version}-#{node.ghc.arch}-unknown-linux.tar.bz2")

remote_file(local_ghc_tarball) do
	source "http://www.haskell.org/ghc/dist/#{node.ghc.version}/ghc-#{node.ghc.version}-#{node.ghc.arch}-unknown-linux.tar.bz2"
	not_if "test -f #{local_ghc_tarball}"
	not_if "ghc --version | grep #{node.ghc.version}"
end

bash "build and install GHC" do
	user "root"
	cwd "/tmp"
	code <<-EOS
		tar jfx #{local_ghc_tarball}
		cd ghc-#{node.ghc.version}
		./configure
		sudo make install
		cd ../
		rm -rf ghc-#{node.ghc.version}
		# rm #{local_ghc_tarball}
	EOS

	creates "/usr/local/bin/ghc"
	not_if "ghc --version | grep #{node.ghc.version}"
end

local_cabal_install_tarball = File.join(td, "cabal-install-1.16.0.2.tar.gz")
remote_file(local_cabal_install_tarball) do
	source "http://hackage.haskell.org/packages/archive/cabal-install/1.16.0.2/cabal-install-1.16.0.2.tar.gz"
	not_if "test -f #{local_cabal_install_tarball}"
	not_if "cabal --version | grep 1.16.0"
end

bash "build and install cabal-install" do
	user "root"
	cwd "/tmp"
	code <<-EOS
		tar xzvf #{local_cabal_install_tarball}
		cd cabal-install-1.16.0.2
		sh bootstrap.sh --global
	EOS

	creates "/usr/local/bin/cabal"
	not_if "cabal --version | grep 1.16.0"
end
