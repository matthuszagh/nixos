final: prev:

{
  linux-gpib = (prev.linux-gpib.override { }).overrideAttrs (oldAttrs: {
    patches = [
      ../pkgs/linux-gpib/linux-gpib-user-4.3.3.patch
    ];
  });
}
