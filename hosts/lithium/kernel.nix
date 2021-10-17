{ config, pkgs, lib, ... }:

pkgs.linuxPackagesFor (pkgs.linux_5_14_hardened.override {
  structuredExtraConfig = with lib.kernel; {
    ACPI_TABLE_UPGRADE = no;
    AIO = lib.mkForce no;
    BINFMT_MISC = yes; # NOTE
    BPF_SYSCALL = lib.mkForce no;
    BUG_ON_DATA_CORRUPTION = yes;
    COMPAT_BRK = no;
    COMPAT_VDSO = no;
    COREDUMP = no;
    CRASH_DUMP = no;
    CRYPTO_JITTERENTROPY = yes;
    DEBUG_FS = no;
    DEBUG_WX = yes;
    DEBUG_VIRTUAL = yes;
    DEVMEM = no;
    DEVPORT = no;
    FORTIFY_SOURCE = yes;
    HARDENED_USERCOPY = yes;
    HARDENED_USERCOPY_FALLBACK = no;
    HARDENED_USERCOPY_PAGESPAN = no;
    HIBERNATION = no;
    INET_DIAG = yes; # NOTE
    INIT_ON_ALLOC_DEFAULT_ON = yes;
    INIT_ON_FREE_DEFAULT_ON = yes;
    KEXEC = no;
    KSM = lib.mkForce no;
    LDISC_AUTOLOAD = no;
    LEGACY_PTYS = no;
    MAGIC_SYSRQ_DEFAULT_ENABLE = freeform "0x84";
    PROC_PAGE_MONITOR = no;
    PAGE_POISONING = no;
    PAGE_SANITIZE = yes;
    PAGE_SANITIZE_VERIFY = yes;
    PROFILING = no;
    RANDOM_TRUST_CPU = lib.mkForce no;
    RANDOM_TRUST_BOOTLOADER = no;
    RESET_ATTACK_MITIGATION = yes;
    SECURITY_LOCKDOWN_LSM = no; # NOTE: depends on sigs
    SLAB_FREELIST_HARDENED = yes;
    SLAB_FREELIST_RANDOM = yes;
    SLAB_MERGE_DEFAULT = no;
    STACKPROTECTOR = yes;
    STACKPROTECTOR_STRONG = yes;
    STAGING = lib.mkForce no;
    SYN_COOKIES = yes;
    SYSFS_SYSCALL = no;
    USELIB = no;
    VMAP_STACK = yes;
    KEYS = yes;
    KEY_DH_OPERATIONS = yes;
    SECURITY_DMESG_RESTRICT = yes;
    SECURITY_PERF_EVENTS_RESTRICT = yes;
    SECURITY_TIOCSTI_RESTIRCT = yes;
    SECURITY_NETWORK_XFRM = yes;
    SECURITY_PATH = yes;

    # NOTE: generate our own key for r13y
    #MODULE_SIG = no;
    #MODULE_SIG_FORCE = no;
    #MODULE_SIG_ALL = yes;
    #MODULE_SIG_SHA512 = yes;
    #MODULE_SIG_KEY = freeform "certs/signing_key.pem";

    AMD_IOMMU_v2 = yes;
    CONFIG_TCG_TPM = yes;
    # FIXME
    #HW_RANDOM_AMD = yes;
    #HW_RANDOM_INTEL = yes;
    HW_RANDOM_TPM = yes;
    INTEL_IOMMU_DEFAULT_ON = yes;
    INTEL_IOMMU_SVM = no;
    INTEL_TXT = no;
    NOUVEAU_LEGACY_CTX_SUPPORT = no;
    INTEL_TH = no;

    ANDROID = no;
    FIREWIRE = no;
    FPGA = no;
    I2C_GPIO = no;
    IIO = no;
    INFINIBAND = no;
    STM = no;
    THUNDERBOLT = no;
    VIDEO_VIVID = no;
    XILLYBUS = no;
    XILLYBUS_CLASS = no;
    XILLYBUS_PCIE = no;
    XILLYUSB = no;
    
    DEFAULT_MMAP_MIN_ADDR = freeform "65536";
    IA32_EMULATION = no;
    LEGACY_VSYSCALL_NONE = yes;
    MODIFY_LDT_SYSCALL = no;
    PAGE_TABLE_ISOLATION = yes;
    RANDOMIZE_BASE = yes;
    RANDOMIZE_KSTACK_OFFSET_DEFAULT = yes;
    RANDOMIZE_MEMORY = yes;
    X86_64 = yes;
    X86_MSR = no;
    X86_VSYSCALL_EMULATION = no;
    X86_X32 = no; # NOTE
  };

  ignoreConfigErrors = true;
})
