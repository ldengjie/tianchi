#!/bin/sh
#g++ 4.9.0
export PATH=/afs/ihep.ac.cn/users/l/lidj/user/software/gcc-4.9.0/bin:$PATH
export LD_LIBRARY_PATH=/afs/ihep.ac.cn/users/l/lidj/user/software/gcc-4.9.0/lib:/afs/ihep.ac.cn/users/l/lidj/user/software/gcc-4.9.0/lib64:/afs/ihep.ac.cn/users/l/lidj/user/software/mpc-0.8.1/lib:/afs/ihep.ac.cn/users/l/lidj/user/software/gmp-4.3.2/lib:/afs/ihep.ac.cn/users/l/lidj/user/software/mpfr-2.4.2/lib:$LD_LIBRARY_PATH
#root-v6
pushd /publicfs/dyb/user/lidj/software/root-v6-00/
source bin/thisroot.sh
popd

cd ~/file/tianchi/jobs

root -q -b -l mode.C > 20.log
