{
  lib,
  pkgs,
}: let
  # Render one carapace spec per command name from a shared definition.
  # JSON is valid spec input and avoids YAML escaping for descriptions.
  render = name: body:
    pkgs.writeText "${name}.yaml" (builtins.toJSON ({
        name = name;
      }
      // body));

  family = common: members:
    lib.mapAttrs' (
      name: extra:
        lib.nameValuePair name (render name (lib.recursiveUpdate common extra))
    )
    members;

  file = ["$files"];
  files = [file];
  dir = ["$directories"];

  # Common flag blocks.
  showHelp = {
    "-h, --help" = "show help";
    "-V, --version" = "show version";
  };
  fsCommon = {
    "-a" = "repair automatically";
    "-f" = "force a check even if the filesystem looks clean";
    "-n" = "check only, do not repair";
    "-p" = "repair automatically (preen mode)";
    "-r" = "repair interactively";
    "-v" = "verbose output";
    "-y" = "repair without asking";
    "-V, --version" = "show version";
    "-h, --help" = "show help";
  };
in
  # ---------------------------------------------------------------- compression
  family {completion.positional = files;} {
    bzcmp = {};
    bzdiff = {};
    zdiff = {};
    zcmp = {};
    xzcmp = {};
    xzdiff = {};
    lzcmp = {};
    lzdiff = {};
  }
  // family {completion.positional = files;} {
    bzless = {};
    bzmore = {};
    zless = {};
    zmore = {};
    xzless = {};
    xzmore = {};
    lzless = {};
    lzmore = {};
    zstdless = {};
  }
  // family {completion.positionalany = file;} {
    xzgrep = {};
    xzegrep = {};
    xzfgrep = {};
    lzgrep = {};
    lzegrep = {};
    lzfgrep = {};
    zstdgrep = {};
    zipgrep = {};
  }
  // family {flags = showHelp;} {
    xzdec.completion.positional = files;
    lzmadec.completion.positional = files;
    lzmainfo.completion.positional = files;
    gzexe = {
      flags = {
        "-d" = "decompress instead of compressing";
        "--help" = "show help";
        "--version" = "show version";
      };
      completion.positional = files;
    };
    zipcloak = {
      flags = {
        "-d, --decrypt" = "decrypt encrypted entries";
        "-b, --temp-path=" = "directory for the temporary zip file";
        "-O, --output-file=" = "write output to a new zip file";
        "-q, --quiet" = "quiet operation";
        "-h, --help" = "show help";
        "-v, --version" = "show version";
        "-L, --license" = "show the license";
      };
      completion.positional = files;
    };
    zipnote = {
      flags = {
        "-w" = "write zipfile comments from stdin";
        "-b=" = "directory for the temporary zip file";
        "-q" = "quiet operation";
        "-v" = "show version";
        "-L" = "show the license";
      };
      completion.positional = files;
    };
    zipsplit = {
      flags = {
        "-t" = "report how many files it will take";
        "-i" = "create an index file";
        "-n=" = "maximum size of each split zip file";
        "-r=" = "room to leave on the first disk";
        "-b=" = "directory for the output zip files";
        "-q" = "quiet operation";
        "-p" = "pause between output files";
        "-s" = "sequential split";
        "-v" = "show version";
        "-L" = "show the license";
      };
      completion.positional = files;
    };
    unzipsfx.completion.positional = files;
  }
  # ---------------------------------------------------------------- netfilter
  // {
    iptables-translate = render "iptables-translate" {
      description = "Translate iptables rules to nftables";
      flags = {
        "-t, --table=" = "table to operate on";
        "-h, --help" = "show help";
        "-V, --version" = "show version";
      };
      completion.positionalany = file;
    };
    iptables-apply = render "iptables-apply" {
      description = "Safely apply an iptables ruleset";
      flags = {
        "-h" = "show help";
        "-V" = "show version";
        "-t, --timeout=" = "rollback timeout";
        "-w, --savefile=" = "save the working ruleset to this file";
        "-c, --runcmd=" = "command to configure iptables";
      };
      completion.positional = files;
    };
    iptables-xml = render "iptables-xml" {
      description = "Convert iptables rules to XML";
      flags = {
        "-c" = "combine";
        "-v, --verbose" = "verbose output";
        "--combine" = "combine consecutive rules";
        "-h, --help" = "show help";
      };
      completion.positional = files;
    };
    xtables-monitor = render "xtables-monitor" {
      description = "Monitor the nftables ruleset";
      flags = {
        "-t, --trace" = "trace ruleset traversal";
        "-e, --event" = "show ruleset change events";
        "-4, --ipv4" = "only monitor IPv4";
        "-6, --ipv6" = "only monitor IPv6";
        "-c, --counters" = "show counters";
        "-h, --help" = "show help";
        "-V, --version" = "show version";
      };
    };
  }
  # ------------------------------------------------------------ fsck / mkfs
  // {
    fsck = render "fsck" {
      description = "Check and repair a filesystem";
      flags = fsCommon;
      completion.positional = files;
    };
    mkfs-fat = render "mkfs-fat" {
      description = "Create a FAT filesystem";
      flags = {
        "-a" = "disable alignment of data structures";
        "-A" = "toggle the Atari variant";
        "-b=" = "location of the FAT32 backup boot sector";
        "-c" = "check the device for bad blocks";
        "-C" = "create the target file";
        "-D=" = "BIOS drive number";
        "-f=" = "number of file allocation tables";
        "-F=" = "FAT size (12, 16 or 32)";
        "-g=" = "disk geometry (heads/sectors_per_track)";
        "-h=" = "hidden sectors";
        "-i=" = "volume ID";
        "-I" = "ignore safety checks";
        "-l=" = "read bad blocks from this file";
        "-m=" = "replace the default error message in the boot block";
        "-M=" = "media type";
        "--mbr?" = "fill the MBR with a partition spanning the whole disk";
        "-n=" = "volume name";
        "--codepage=" = "DOS codepage for the label";
        "-r=" = "root directory entries";
        "-R=" = "minimal number of reserved sectors";
        "-s=" = "sectors per cluster";
        "-S=" = "logical sector size";
        "-v" = "verbose execution";
        "--variant=" = "filesystem variant (standard or Atari)";
        "--invariant" = "use fixed timestamps";
        "--offset=" = "write the filesystem at this sector offset";
        "--help" = "show help";
      };
      completion.positional = files;
    };
    mkfs-ntfs = render "mkfs-ntfs" {
      description = "Create an NTFS filesystem";
      flags = {
        "-f, --fast" = "perform a quick format";
        "-Q, --quick" = "perform a quick format";
        "-L, --label=" = "volume label";
        "-C, --enable-compression" = "enable compression";
        "-I, --no-indexing" = "disable indexing";
        "-n, --no-action" = "do not write to disk";
        "-c, --cluster-size=" = "cluster size";
        "-s, --sector-size=" = "sector size";
        "-p, --partition-start=" = "partition start sector";
        "-H, --heads=" = "number of heads";
        "-S, --sectors-per-track=" = "sectors per track";
        "-z, --mft-zone-multiplier=" = "MFT zone multiplier";
        "-T, --zero-time" = "use a fixed timestamp";
        "-F, --force" = "force execution despite errors";
        "-q, --quiet" = "quiet execution";
        "-v, --verbose" = "verbose execution";
        "--debug" = "very verbose execution";
        "-V, --version" = "show version";
        "-l, --license" = "show licensing information";
        "-h, --help" = "show help";
      };
      completion.positional = files;
    };
  }
  # ---------------------------------------------------------------- blockfs
  // family {
    flags =
      {
        "-f, --force" = "use less caution";
        "-q, --quiet" = "less output";
        "-v, --verbose" = "more output";
        "-n, --no-action" = "do not write to disk";
        "-h, --help" = "show help";
        "-V, --version" = "show version";
      };
    completion.positional = files;
  } {
    "ntfs-3g" = {
      flags = {
        "-o=" = "mount options";
      };
      completion.positional = [file dir];
    };
    ntfs-probe = {
      flags = {
        "-m" = "mount mode";
        "-w" = "read-write mode";
        "-r" = "read-only mode";
      };
    };
    ntfscat = {
      flags = {
        "-a, --attribute=" = "attribute type";
        "--attribute-name=" = "attribute name";
        "-i, --inode=" = "inode number";
      };
      completion.positional = [file []];
    };
    ntfsck = {};
    ntfsclone = {
      flags = {
        "-o, --output=" = "clone to this file";
        "-O, --overwrite=" = "clone to this file, overwriting it";
        "-s, --save-image" = "save to the special image format";
        "-r, --restore-image" = "restore from the special image format";
        "--rescue" = "continue after disk read errors";
        "-m, --metadata" = "clone only metadata";
        "--ignore-fs-check" = "ignore the filesystem check result";
        "--new-serial" = "set a new serial number";
        "--new-half-serial" = "set a partial new serial number";
        "-S, --adjust-sector-size" = "adjust the sector size";
        "-t, --preserve-timestamps" = "preserve timestamps";
      };
    };
    ntfscluster = {};
    ntfscmp = {};
    ntfscp = {
      flags = {
        "-a, --attribute=" = "attribute number";
        "-i, --inode" = "treat the destination as an inode";
        "-m, --min_fragments" = "minimal fragmentation";
        "-N, --attr-name=" = "attribute name";
        "-t, --timestamp" = "copy the modification time";
      };
      completion.positional = [file file file];
    };
    ntfsfix = {
      flags = {
        "-b, --clear-bad-sectors" = "clear the bad sector list";
        "-d, --clear-dirty" = "clear the volume dirty flag";
      };
    };
    ntfsinfo = {
      flags = {
        "-i, --inode=" = "inode number";
        "-F, --file=" = "absolute file path";
        "-m, --mft" = "dump MFT information";
        "-t, --notime" = "do not report timestamps";
      };
    };
    ntfslabel = {
      flags = {
        "--new-serial" = "set a new serial number";
        "--new-half-serial" = "set a partial new serial number";
      };
      completion.positional = [file []];
    };
    ntfsls = {
      flags = {
        "-a, --all" = "show all files";
        "-F, --classify" = "classify entries";
        "-i, --inode" = "show inode numbers";
        "-l, --long" = "long listing";
        "-p, --path=" = "directory to list";
        "-R, --recursive" = "recurse into subdirectories";
        "-s, --system" = "show system files";
        "-x, --dos" = "use short DOS names";
      };
    };
    ntfsrecover = {};
    ntfsresize = {
      flags = {
        "-c, --check" = "check whether the device is ready to resize";
        "-i, --info" = "estimate the smallest shrink or expansion size";
        "-m, --info-mb-only" = "estimate the smallest size in MB";
        "-s, --size=" = "new size";
        "-x, --expand" = "expand to the full partition";
        "-b, --bad-sectors" = "support disks with bad sectors";
        "-P, --no-progress-bar" = "do not show a progress bar";
      };
    };
    ntfssecaudit = {};
    ntfstruncate = {};
    ntfsundelete = {
      flags = {
        "-s, --scan" = "scan for files";
        "-p, --percentage=" = "minimum percentage recoverable";
        "-m, --match=" = "only files matching this pattern";
        "-C, --case" = "case-sensitive matching";
        "-S, --size=" = "match files of this size";
        "-t, --time=" = "last referenced since this time";
        "-u, --undelete" = "undelete mode";
        "-i, --inodes=" = "inodes to recover";
        "-o, --output=" = "output file name";
        "-O, --optimistic" = "undelete in-use clusters too";
        "-d, --destination=" = "destination directory";
        "-b, --byte=" = "fill missing parts with this byte";
        "-T, --truncate" = "truncate to the exact size";
        "-P, --parent" = "show the parent directory";
      };
    };
    ntfsusermap = {};
    ntfswipe = {};
  }
  // family {
    flags =
      {
        "-h" = "show help";
        "-V" = "show version";
        "-d=" = "debug level";
        "-q" = "quiet output";
        "-v" = "verbose output";
      };
    completion.positional = files;
  } {
    f2fs-fsck = {
      flags = {
        "-a" = "check and fix potential corruption";
        "-f" = "check and fix the entire partition";
        "-l" = "show the superblock and checkpoint";
        "-M" = "show a file map";
        "-t" = "show the directory tree";
        "-y" = "fix everything";
        "--dry-run" = "do not fix anything";
      };
    };
    f2fs-dump = {};
    f2fs-resize = {};
    f2fslabel = {};
    f2fs-mkfs = {
      flags = {
        "-f" = "force overwrite of an existing filesystem";
        "-l" = "volume label";
        "-U" = "UUID";
        "-O=" = "features to enable";
        "-C=" = "casefolding options";
        "-q" = "quiet mode";
        "-S" = "sparse mode";
        "-T" = "timestamps";
      };
    };
    f2fs-sload = {
      flags = {
        "-t=" = "target directory";
        "-d=" = "debug level";
        "-i=" = "input file";
        "-o=" = "offset";
        "-s=" = "number of segments";
        "-n=" = "number of files";
        "-p=" = "number of threads";
        "-c=" = "compression";
        "-f" = "force";
        "-r" = "restore";
      };
    };
    f2fs_io = {
      completion.positionalany = ["$executables"];
    };
    f2fscrypt = {
      flags = {
        "-e" = "encrypt";
        "-d" = "decrypt";
        "-k=" = "key";
        "-v" = "verbose";
      };
    };
  }
  // family {
    flags =
      {
        "-V, --version" = "show version";
        "-h, --help" = "show help";
        "-v, --verbose" = "verbose output";
        "-q, --quiet" = "quiet output";
      };
    completion.positional = files;
  } {
    exfat-mkfs = {};
    exfat-fsck = {};
    exfat-dump = {
      flags = {
        "-d, --dentry-set=" = "show the directory entry set at this path";
        "-c, --cluster-chain" = "show the cluster chain";
        "-s, --scan-dir=" = "scan this directory";
        "-r, --recursive" = "scan recursively";
      };
    };
    exfat-tune = {
      flags = {
        "-l, --print-label" = "print the volume label";
        "-L, --set-label=" = "set the volume label";
        "-u, --print-guid" = "print the volume GUID";
        "-U, --set-guid=" = "set the volume GUID";
        "-i, --print-serial" = "print the volume serial";
        "-I, --set-serial=" = "set the volume serial";
      };
    };
    exfatlabel = {
      flags = {
        "-i, --volume-serial" = "switch to volume serial mode";
      };
    };
    exfat2img = {
      flags = {
        "-o, --output=" = "destination image file";
      };
    };
  }
  # ---------------------------------------------------------------- bcachefs
  // family {
    flags = {
      "-h, --help" = "show help";
      "-V, --version" = "show version";
    };
    completion.positional = files;
  } {
    bcachefs-format = {
      flags = {
        "--block_size=" = "filesystem block size";
        "--btree_node_size=" = "btree node size";
        "--errors=" = "action on filesystem error";
        "--metadata_replicas=" = "number of metadata replicas";
        "--data_replicas=" = "number of data replicas";
        "--compression=" = "compression algorithm";
        "--background_compression=" = "background compression algorithm";
        "--label=" = "filesystem label";
        "-f, --force" = "force formatting";
      };
    };
    bcachefs-fsck = {
      flags = {
        "-f, --force" = "force a check";
        "-n, --no-repair" = "do not repair";
        "-y, --yes" = "repair without asking";
      };
    };
    bcachefs-mount = {
      flags = {
        "-o=" = "mount options";
      };
      completion.positional = [file dir];
    };
  }
  # ---------------------------------------------------------------- systemd
  // family {
    flags = {
      "-h, --help" = "show help";
      "--version" = "show package version";
    };
  } {
    "systemd-mount" = {
      flags = {
        "-H, --host=" = "operate on a remote host";
        "-M, --machine=" = "operate on a local container";
        "--user" = "run as a user unit";
        "--no-block" = "do not wait for the operation to finish";
        "--no-pager" = "do not start a pager";
        "--no-legend" = "do not show headers and footers";
        "-l, --full" = "do not ellipsize output";
        "-q, --quiet" = "suppress informational messages";
        "-t, --type=" = "filesystem type";
        "-o, --options=" = "mount options";
        "--fsck=" = "run fsck before mounting";
        "--description=" = "unit description";
        "--automount=" = "create an automount unit";
        "--timeout-idle-sec=" = "automount idle timeout";
        "--tmpfs" = "mount a tmpfs";
        "--list" = "list mount units";
        "--umount" = "unmount";
        "--discover" = "discover block devices";
        "--property=" = "unit property";
        "--uuid=" = "filesystem UUID";
        "--label=" = "filesystem label";
        "-p, --partition=" = "partition number";
        "--owner=" = "file ownership";
        "--group=" = "file group";
      };
      completion.positional = [file dir];
    };
    "systemd-umount" = {};
    "systemd-escape" = {
      flags = {
        "--suffix=" = "unit suffix to append";
        "--template=" = "insert the string into this template";
        "--stdin" = "read strings from stdin";
        "--instance" = "show only the instance part";
        "-u, --unescape" = "unescape strings";
        "-m, --mangle" = "mangle strings";
        "-p, --path" = "treat the string as a path";
      };
      completion.positionalany = ["$files" "$directories"];
    };
    "systemd-firstboot" = {
      flags = {
        "--root=" = "operate on an alternate filesystem root";
        "--image=" = "operate on a disk image";
        "--locale=" = "primary locale";
        "--locale-messages=" = "message locale";
        "--keymap=" = "keymap";
        "--timezone=" = "timezone";
        "--hostname=" = "hostname";
        "--setup-machine-id" = "set a random machine ID";
        "--machine-id=" = "set the machine ID";
        "--root-password=" = "root password";
        "--root-password-file=" = "root password file";
        "--root-password-hashed=" = "hashed root password";
        "--root-shell=" = "root shell";
        "--welcome=" = "welcome message";
      };
    };
    "systemd-repart" = {
      flags = {
        "--no-pager" = "do not start a pager";
        "--no-legend" = "do not show headers and footers";
        "-n, --dry-run" = "dry run";
        "--empty=" = "how to handle empty disks";
        "--offline=" = "build the image offline";
        "--discard=" = "discard backing blocks";
        "--sector-size=" = "logical sector size";
        "--definitions=" = "repart definition directory";
        "--seed=" = "seed for partition UUIDs";
        "--split=" = "split the image";
        "--size=" = "image size";
        "--copy-from=" = "copy from an image";
      };
      completion.positional = files;
    };
    "systemd-sysusers" = {
      flags = {
        "--cat-config" = "show configuration files";
        "--tldr" = "show non-comment configuration";
        "--root=" = "operate on an alternate filesystem root";
        "--image=" = "operate on a disk image";
        "--replace=" = "replace a configuration file";
        "-n, --dry-run" = "dry run";
        "--inline" = "treat arguments as configuration lines";
      };
      completion.positionalany = file;
    };
    "systemd-cryptsetup" = {
      completion.positional = [["attach" "detach"]];
      completion.positionalany = file;
    };
    "systemd-socket-activate" = {
      flags = {
        "-l, --listen=" = "listen address";
        "-d, --datagram" = "use a datagram socket";
        "--seqpacket" = "use SOCK_SEQPACKET";
        "-a, --accept" = "spawn a child per connection";
        "-E, --setenv=" = "environment variable for children";
        "--fdname=" = "file descriptor names";
        "--inetd" = "use the inetd protocol";
        "--now" = "start instantly";
      };
      completion.positionalany = ["$executables" "$files"];
    };
    "systemd-ac-power" = {
      flags = {
        "-v, --verbose" = "show state as text";
        "--low" = "check for low battery";
      };
    };
  }
  # ---------------------------------------------------------------- pipewire
  // family {
    flags = {
      "-h, --help" = "show help";
      "--version" = "show version";
      "-R, --remote=" = "remote daemon name";
    };
    completion.positional = files;
  } {
    "pw-cat" = {
      flags = {
        "-v, --verbose" = "verbose operations";
        "--media-type=" = "media type";
        "--media-category=" = "media category";
        "--media-role=" = "media role";
        "--target=" = "node target serial or name";
        "--latency=" = "node latency";
        "-P, --properties=" = "node properties";
        "--rate=" = "sample rate";
        "--channels=" = "number of channels";
        "--channel-map=" = "channel map";
        "--format=" = "sample format";
      };
    };
  }
  // family {
    flags = {
      "-h, --help" = "show help";
      "--version" = "show version";
      "-r, --remote=" = "remote daemon name";
    };
  } {
    "pw-cli" = {
      flags = {
        "-d, --daemon" = "start as a daemon";
        "-m, --monitor" = "monitor activity";
      };
      completion.positional = [["help" "list-vars" "load-module" "unload-module" "connect" "disconnect" "list-remotes" "switch-remote" "list-objects" "info" "create-device" "create-node" "destroy" "create-link" "export-node" "enum-params" "set-param" "permissions" "get-permissions" "client-permissions" "dump" "quit"]];
    };
    "pw-config" = {
      flags = {
        "-n, --name=" = "config name";
        "-p, --prefix=" = "config prefix";
        "-L, --no-newline" = "omit newlines after values";
        "--recurse" = "reformat recursively";
        "-N, --no-colors" = "disable color output";
        "-C, --color=" = "color mode";
      };
      completion.positional = [["paths" "list" "merge"]];
    };
    "pw-container" = {
      flags = {
        "-P, --properties=" = "context properties";
      };
      completion.positionalany = ["$executables"];
    };
    "pw-dot" = {
      flags = {
        "-a, --all" = "show all object types";
        "-s, --smart" = "show linked objects only";
        "-d, --detail" = "show all object properties";
        "-o, --output=" = "output file";
        "-L, --lr" = "use left-right rank direction";
        "-9, --90" = "use orthogonal edges";
        "-j, --json" = "read objects from a pw-dump JSON file";
      };
    };
    "pw-dump" = {
      flags = {
        "-m, --monitor" = "monitor changes";
        "-N, --no-colors" = "disable color output";
        "-C, --color=" = "color mode";
        "-R, --raw" = "force raw output";
        "-i, --indent=" = "indentation amount";
        "-s, --spa" = "SPA JSON output";
      };
    };
    "pw-link" = {
      flags = {
        "-o, --output" = "list output ports";
        "-i, --input" = "list input ports";
        "-l, --links" = "list links";
        "-t, --latency" = "list port latencies";
        "-m, --monitor" = "monitor links and ports";
        "-I, --id" = "list IDs";
        "-L, --linger" = "linger";
        "-P, --passive" = "create a passive link";
        "-p, --props=" = "link properties as JSON";
        "-w, --wait" = "wait for the link attempt";
        "-d, --disconnect" = "disconnect ports";
      };
    };
    "pw-loopback" = {
      flags = {
        "-n, --name=" = "node name";
        "-g, --group=" = "node group";
        "-c, --channels=" = "number of channels";
        "-m, --channel-map=" = "channel map";
        "-l, --latency=" = "desired latency";
        "-d, --delay=" = "desired delay";
        "-C, --capture=" = "capture source";
        "-i, --capture-props=" = "capture stream properties";
        "-P, --playback=" = "playback sink";
        "-o, --playback-props=" = "playback stream properties";
      };
    };
    "pw-metadata" = {
      flags = {
        "-l, --list" = "list available metadata";
        "-m, --monitor" = "monitor metadata";
        "-d, --delete" = "delete metadata";
        "-n, --name=" = "metadata name";
      };
    };
    "pw-mididump" = {
      flags = {
        "-M, --force-midi=" = "force midi or ump format";
      };
      completion.positional = files;
    };
    "pw-mon" = {
      flags = {
        "-N, --no-colors" = "disable color output";
        "-C, --color=" = "color mode";
        "-o, --hide-props" = "hide node properties";
        "-a, --hide-params" = "hide node parameters";
        "-p, --print-separator" = "print a separator after every event";
      };
    };
    "pw-profiler" = {
      flags = {
        "-o, --output=" = "profiler output name";
        "-J, --json" = "dump raw data as JSON";
        "-n, --iterations=" = "number of samples";
      };
    };
    "pw-reserve" = {
      flags = {
        "-n, --name=" = "name to reserve";
        "-a, --appname=" = "application name";
        "-p, --priority=" = "priority";
        "-m, --monitor" = "monitor only";
        "--release" = "request release when busy";
      };
    };
    "pw-top" = {
      flags = {
        "-b, --batch-mode" = "batch mode";
        "-n, --iterations=" = "number of iterations";
      };
    };
    "pw-v4l2" = {
      flags = {
        "-v" = "verbose debug output";
      };
      completion.positionalany = ["$executables" "$files"];
    };
  }
  # ---------------------------------------------------------------- gimp
  // family {
    flags = {
      "-h, --help" = "show help";
      "-v, --version" = "show version";
      "--verbose" = "be more verbose";
      "-n, --new-instance" = "start a new instance";
      "-a, --as-new" = "open images as new";
      "-d, --no-data" = "do not load brushes, gradients or patterns";
      "-f, --no-fonts" = "do not load fonts";
      "-s, --no-splash" = "do not show a splash screen";
      "--no-shm" = "do not use shared memory";
      "--no-cpu-accel" = "disable CPU acceleration";
      "-g, --gimprc=" = "alternate gimprc";
      "--system-gimprc=" = "alternate system gimprc";
      "-b, --batch=" = "batch command";
      "--batch-interpreter=" = "batch interpreter";
      "--quit" = "quit after the requested actions";
      "-c, --console-messages" = "send messages to the console";
      "--pdb-compat-mode=" = "PDB compatibility mode";
      "--stack-trace-mode=" = "stack trace mode";
    };
    completion.positional = files;
  } {
    "gimp-console-3" = {};
  }
  // family {
    flags = {
      "--help" = "show help";
      "--quiet" = "do not echo build commands";
      "--silent" = "do not echo build commands";
      "--version" = "show the GIMP version";
      "-n, --dry-run" = "print commands without running them";
      "--just-print" = "print commands without running them";
      "--recon" = "print commands without running them";
      "--cflags" = "print compiler flags";
      "--libs" = "print linker flags";
      "--prefix=" = "installation prefix";
      "--exec-prefix=" = "installation exec prefix";
      "--msvc-syntax" = "print flags in MSVC syntax";
    };
    completion.positional = files;
  } {
    gimptool-3 = {};
  }
  // family {
    flags = {
      "-h, --help" = "show help";
      "-s, --selection-type=" = "selection type";
      "-l, --list-targets" = "list clipboard targets";
      "-t, --target=" = "target format";
      "-c, --copy=" = "copy a file to the clipboard";
      "-S, --store=" = "store a file in the clipboard manager";
      "-p, --paste=" = "paste the clipboard into a file";
    };
    completion.positional = files;
  } {
    gimp-test-clipboard-3 = {};
  }
  # ---------------------------------------------------------------- monero
  // family {
    flags = {
      "--help" = "produce help message";
      "--version" = "output version information";
      "--testnet" = "run on testnet";
      "--stagenet" = "run on stagenet";
      "--regtest" = "run in regression test mode";
      "--config-file=" = "configuration file";
      "--data-dir=" = "data directory";
      "--log-file=" = "log file";
      "--log-level=" = "log level";
      "--max-log-file-size=" = "maximum log file size";
      "--max-log-files=" = "maximum number of log files";
      "--proxy=" = "SOCKS proxy";
      "--trusted-daemon" = "enable commands that rely on a trusted daemon";
      "--untrusted-daemon" = "disable commands that rely on a trusted daemon";
      "--daemon-address=" = "daemon address";
      "--daemon-host=" = "daemon host";
      "--daemon-port=" = "daemon port";
      "--daemon-login=" = "daemon RPC credentials";
      "--daemon-ssl=" = "daemon RPC SSL mode";
      "--offline" = "offline mode";
      "--enforce-dns-checkpointing" = "enforce DNS checkpointing";
      "--enable-dns-blocklist" = "enable the DNS blocklist";
    };
  } {
    "monero-wallet-cli" = {
      flags = {
        "--wallet-file=" = "wallet file";
        "--generate-new-wallet=" = "generate a new wallet";
        "--password=" = "wallet password";
        "--password-file=" = "wallet password file";
        "--restore-deterministic-wallet" = "restore a deterministic wallet";
        "--restore-height=" = "restore height";
        "--seed=" = "wallet seed";
        "--language=" = "wallet language";
        "--use-english-language-names" = "use English language names";
        "--shared-ringdb-dir=" = "shared ring database directory";
        "--kdf-rounds=" = "key derivation rounds";
        "--bitmessage-address=" = "PyBitmessage address";
        "--bitmessage-login=" = "PyBitmessage credentials";
        "--subaddress-lookahead=" = "subaddress lookahead";
        "--allow-mismatched-daemon-version" = "allow a mismatched daemon version";
      };
      completion.positional = files;
    };
    "monero-wallet-rpc" = {
      flags = {
        "--rpc-bind-ip=" = "RPC bind address";
        "--rpc-bind-port=" = "RPC bind port";
        "--confirm-external-bind" = "confirm binding to an external address";
        "--restricted-rpc" = "restrict RPC calls";
        "--wallet-dir=" = "wallet directory";
        "--wallet-file=" = "wallet file";
        "--password=" = "wallet password";
        "--disable-rpc-login" = "disable RPC login";
        "--rpc-login=" = "RPC credentials";
        "--rpc-access-control-origins=" = "allowed CORS origins";
        "--shared-ringdb-dir=" = "shared ring database directory";
      };
    };
    "monero-blockchain-export" = {
      flags = {
        "--output-file=" = "output file";
        "--block-start=" = "first block";
        "--block-stop=" = "last block";
        "--blocksdat" = "output in blocks.dat format";
      };
      completion.positional = files;
    };
    "monero-blockchain-import" = {
      flags = {
        "--input-file=" = "input file";
        "--block-start=" = "first block";
        "--block-stop=" = "last block";
        "--batch-size=" = "batch size";
        "--db-type=" = "database type";
        "--prune" = "prune the blockchain";
        "--blocksdat" = "input is in blocks.dat format";
      };
      completion.positional = files;
    };
    "monero-blockchain-ancestry" = {
      flags = {
        "--output-file=" = "output file";
        "--block-start=" = "first block";
        "--block-stop=" = "last block";
      };
      completion.positional = files;
    };
    "monero-blockchain-depth" = {
      flags = {
        "--output-file=" = "output file";
        "--block-start=" = "first block";
        "--block-stop=" = "last block";
      };
      completion.positional = files;
    };
    "monero-blockchain-stats" = {
      flags = {
        "--output-file=" = "output file";
        "--block-start=" = "first block";
        "--block-stop=" = "last block";
      };
      completion.positional = files;
    };
    "monero-blockchain-usage" = {
      flags = {
        "--output-file=" = "output file";
      };
      completion.positional = files;
    };
    "monero-blockchain-prune" = {};
    "monero-blockchain-prune-known-spent-data" = {};
    "monero-blockchain-mark-spent-outputs" = {
      flags = {
        "--output-file=" = "output file";
      };
      completion.positional = files;
    };
    "monero-gen-ssl-cert" = {
      flags = {
        "--certificate-filename=" = "certificate file";
        "--private-key-filename=" = "private key file";
        "--passphrase=" = "private key passphrase";
        "--passphrase-file=" = "file containing the passphrase";
        "--prompt-for-passphrase" = "prompt for the passphrase";
      };
    };
    "monero-gen-trusted-multisig" = {
      flags = {
        "--multisig-file=" = "multisig file";
        "--threshold=" = "signature threshold";
        "--scheme=" = "signature scheme";
        "--keys-file=" = "keys file";
      };
      completion.positional = files;
    };
  }
