# ---
# Source: https://blog.zwindler.fr/2020/02/10/redis-mongodb-rabbitmq-desactiver-les-transparent-huge-pages/
# ---
apt-get install -y sysfsutils
echo "kernel/mm/transparent_hugepage/enabled = never" >> /etc/sysfs.conf
# No need to reboot doing the following command:
echo "never" >> /sys/kernel/mm/transparent_hugepage/enabled


# ---
# https://github.com/moby/moby/issues/19758
# ---
{
echo '###################################################################';
echo '# Kernel softlockup panic => kernel panic ';
echo 'kernel.softlockup_panic=1      # this will panic on soft lockup';
echo 'kernel.panic=60                # reset system on panic after 60 seconds';
} >> /etc/sysctl.conf
# No need to reboot doing the following commands:
sysctl kernel.softlockup_panic=1      # this will panic on soft lockup
sysctl kernel.panic=60                # reset system on panic after 60 seconds

# -------------------------------- #
#      SWAP and STORAGE limits     #
# -------------------------------- #

# ---
# https://docs.docker.com/storage/storagedriver/overlayfs-driver/
# "The overlay2 drivers are supported on xfs backing filesystems, but only with d_type=true enabled.
#  Use xfs_info to verify that the ftype option is set to 1. To format an xfs filesystem correctly, use the flag -n ftype=1"
# ---
# So, the output of the following function should be something like this:
#   ---
#   naming  =version 2  bsize=4096  ascii-ci=0, ftype=1
#
#   Storage Driver: overlay2
#    Backing Filesystem: xfs
#    Supports d_type: true
#    Native Overlay Diff: true
#
#   /dev/sda1 / xfs rw,relatime,attr2,inode64,noquota 0 0             <= BAD with "noquota"
#   /dev/sda1 / xfs rw,relatime,attr2,inode64,usrquota,prjquota 0 0   <= CORRECT
#   ---
# otherwise you have to format again your partition with something like this:
#   ---
#   mkfs.xfs -f -n ftype=1 /dev/sda1
#   ---
function mariotel_check_host_filesystem {
  xfs_info /  | grep ftype
  echo
  docker info | \grep 'Storage Driver' -A 3
  echo
  grep "$(df '/var/lib/docker' -t xfs --output='source' | tail -n 1)" /proc/mounts
}

# ---
# Sources:
# https://fr-wiki.ikoula.com/fr/Comment_activer_la_gestion_swap_dans_les_cgroups
# https://fabianlee.org/2020/01/15/docker-use-overlay2-with-an-xfs-backing-filesystem-to-limit-rootfs-size/
# ---
### Edit and update the grub configuration:
# nano /etc/default/grub
# GRUB_CMDLINE_LINUX_DEFAULT="rootflags=uquota,pquota quiet"
# GRUB_CMDLINE_LINUX="cgroup_enable=memory swapaccount=1"
# grub-mkconfig -o /boot/grub/grub.cfg
### We need to reboot:
# reboot


