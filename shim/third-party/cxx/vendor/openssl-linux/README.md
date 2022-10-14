Vendor of Ubuntu 22.04 libssl-dev

Version obtained with
```
apt show libssl-dev | grep '^Version'
```
Version: 3.0.2-0ubuntu1.6

Files obtained with
```
dpkg -L libssl-dev \
| grep -e '/usr/include/.*\.h\|/usr/lib/.*/\(libcrypto.a\|libssl.a\)' \
| tar cT - --transform='s|usr/||' \
| tar xvf - -C shim/third-party/cxx/vendor/openssl-linux/
```
