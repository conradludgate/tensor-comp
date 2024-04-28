version = $(version)

.llvm/llvm-project-$(version).src:
	curl -L -o .llvm/llvm-project-$(version).src.tar.xz https://github.com/llvm/llvm-project/releases/download/llvmorg-$(version)/llvm-project-$(version).src.tar.xz
	gpg --import .llvm/release-keys.asc
	gpg --verify .llvm/llvm-project-$(version).src.tar.xz.sig .llvm/llvm-project-$(version).src.tar.xz
	tar xf .llvm/llvm-project-$(version).src.tar.xz --directory=.llvm
	rm .llvm/llvm-project-$(version).src.tar.xz

.llvm/llvm-project-$(version).build: .llvm/llvm-project-$(version).src
	mkdir -p .llvm/llvm-project-$(version).build
	cmake -S .llvm/llvm-project-$(version).src/llvm -B .llvm/llvm-project-$(version).build -G 'Unix Makefiles' -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX="$(shell pwd)/.llvm/llvm-project-$(version).install"

.llvm/llvm-project-$(version).install: .llvm/llvm-project-$(version).build
	mkdir .llvm/llvm-project-$(version).install
	$(MAKE) -C .llvm/llvm-project-$(version).build install

.PHONY: clean
clean:
	rm -rf .llvm/llvm-project-$(version).install
	rm -rf .llvm/llvm-project-$(version).build
	rm -rf .llvm/llvm-project-$(version).src
