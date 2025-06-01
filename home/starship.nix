{ lib, ... }:

{
  programs.starship = {
    enable = true;
    enableZshIntegration = false;

    settings = {
      add_newline = false;
      continuation_prompt = ">> ";

      format = lib.concatStrings [
        "$username"
        "$hostname"
        "$localip"
        "$directory"
        "$git_branch"
        "$git_commit"
        "$git_state"
        "$git_status"
        "$fill"
        "$git_metrics"
        "$fill "
        "$time"
        "$shlvl"
        "$status"
        "$shell"
        "$all"
        "$character"
      ];

      aws = {
        format = "[$symbol( $profile)($region)($duration)]($style) ";
        symbol = "aws";
      };

      c = {
        format = "[$symbol( $version(-$name))]($style) ";
        symbol = "c";
      };

      character = {
        success_symbol = "[>](green)";
        error_symbol = "[>](red)";
        vimcmd_symbol = "[<](green)";
      };

      cmake = {
        format = "[$symbol( $version)]($style) ";
        symbol = "cmake";
      };

      cmd_duration = {
        format = "[$duration]($style) ";
      };

      conda = {
        format = "[$symbol $environment]($style) ";
        symbol = "conda";
      };

      dart = {
        format = "[$symbol( $version)]($style) ";
        symbol = "dart";
      };

      docker_context = {
        format = "[$symbol $context]($style) ";
        symbol = "docker";
      };

      fill = {
        symbol = "-";
        style = "dimmed white";
      };

      gcloud = {
        format = "[$symbol $account(@$domain)($region)]($style) ";
        symbol = "gcp";
      };

      git_branch = {
        format = "[$branch]($style) ";
      };

      git_commit = {
        tag_symbol = " tag ";
      };

      git_status = {
        format = "([$all_status$ahead_behind]($style))";
        style = "yellow";

        diverged = "<$behind_count>$ahead_count";

        conflicted = "=$count ";
        ahead = "[>$count](green) ";
        behind = "[<$count](green) ";
        untracked = "?$count ";
        stashed = "*$count ";
        modified = "!$count ";
        staged = "+$count ";
        renamed = "r$count ";
        deleted = "x$count ";
      };

      git_metrics = {
        disabled = false;
        format = "( [+$added]($added_style) )([-$deleted]($deleted_style) )";
      };

      golang = {
        format = "[$symbol( $version)]($style) ";
        symbol = "go";
      };

      gradle = {
        format = "[$symbol( $version)]($style) ";
        symbol = "gradle";
      };

      haskell = {
        format = "[$symbol( $version)]($style) ";
        symbol = "hs";
      };

      helm = {
        format = "[$symbol( $version)]($style) ";
        symbol = "helm";
      };

      java = {
        format = "[$symbol( $version)]($style) ";
        symbol = "java";
      };

      julia = {
        format = "[$symbol( $version)]($style) ";
        symbol = "jl";
      };

      kotlin = {
        format = "[$symbol( $version)]($style) ";
        symbol = "kt";
      };

      kubernetes = {
        format = "[$symbol $context( $namespace)]($style) ";
        symbol = "k8s";
      };

      lua = {
        format = "[$symbol( $version)]($style) ";
        symbol = "lua";
      };

      memory_usage = {
        format = "$symbol [$ram( | $swap)]($style) ";
        symbol = "ram";
      };

      meson = {
        format = "[$symbol $project]($style) ";
        symbol = "meson";
      };

      nix_shell = {
        format = "[$symbol $state( $name)]($style) ";
        symbol = "nix";
      };

      nodejs = {
        format = "[$symbol( $version)]($style) ";
        symbol = "node";
      };

      os = {
        format = "[$symbol]($style) ";
      };

      package = {
        format = "[$symbol $version]($style) ";
        symbol = "pkg";
      };

      purescript = {
        format = "[$symbol( $version)]($style) ";
        symbol = "purs";
      };

      python = {
        format = "[$symbol$pyenv_prefix( $version)($virtualenv)]($style) ";
        symbol = "py";
      };

      ruby = {
        format = "[$symbol( $version)]($style) ";
        symbol = "rb";
      };

      rust = {
        format = "[$symbol( $version)]($style) ";
        symbol = "rs";
      };

      shell = {
        disabled = false;
      };

      shlvl = {
        disabled = false;
        symbol = "lvl ";
      };

      status = {
        disabled = false;
        format = "[$common_meaning]($style) ";
        # TODO: Fix the weird width stuff
        pipestatus = false;
        pipestatus_format = "$pipestatus=> [$common_meaning$signal_name$maybe_int]($style) ";
      };

      sudo = {
        format = "[as $symbol]($style) ";
        symbol = "sudo";
      };

      swift = {
        format = "[$symbol( $version)]($style) ";
        symbol = "swift";
      };

      terraform = {
        format = "[$symbol $workspace]($style) ";
        symbol = "tf";
      };

      time = {
        disabled = false;
        format = "[$time]($style) ";
      };

      username = {
        format = "[$user]($style) ";
      };

      zig = {
        format = "[$symbol( $version)]($style) ";
        symbol = "zig";
      };
    };
  };
}
