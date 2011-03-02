%define erly_user erlyvideo
%define erly_group erlyvideo

Summary: Erlyvideo multiprotocol streaming server
Name: erlyvideo
Version: 2.4.4
Release: 3%{?dist}
License: GPL
Group: Network
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
Source: http://debian.erlyvideo.org/tgz/erlyvideo-%{version}.tar.gz
URL: http://erlyvideo.org/
BuildRequires: erlang, ruby
Requires: erlang


%description 
Erlyvideo is a multiprotocol opensource videostreaming server
written with efficiency in mind


%prep
%setup -q


%build
make


%pre
%{_sbindir}/groupadd %{erly_group}
%{_sbindir}/useradd -g %{erly_group} -c "ErlyVideo User" -d %{_localstatedir}/lib/%{name} -r -m %{erly_user} 2>/dev/null || :


%install
rm -rf %{buildroot}

# Ownership will be changed after package install (see "post" section)
# So we comment it out in Makefile "install" section
sed -i 's|chown|#chown|' Makefile

# Also, init-script for RHEL/Centos MUST be installed to /etc/rc.d/init.d instead of /etc/init.d
# Otherwise there will be conflict with chkconfig package
sed -i 's|/etc/init.d/|/etc/rc.d/init.d/|' Makefile

make DESTROOT=%{buildroot} install


%post
/sbin/chkconfig --add %{name}
/sbin/chkconfig %{name} on


%files
%defattr(-, root, root)
%{_bindir}/*
%{_libdir}/erlang
%{_initrddir}/%{name}
%config(noreplace) %{_sysconfdir}/%{name}
%doc README.md
%attr(-, %{erly_user}, %{erly_group}) %{_localstatedir}/lib/%{name}
%attr(-, %{erly_user}, %{erly_group}) %{_localstatedir}/log/%{name}
%attr(-, %{erly_user}, %{erly_group}) %{_localstatedir}/cache/%{name}


%clean
rm -rf %{buildroot}


%changelog
* Mon Feb 21 2011 Pavel Derendyaev <paul@reic.ru> - 2.4.4-3
- Create erlyvideo as system user
- Add /var/log/erlyvideo dir
- Use "attr" macro instead of "chown" command

* Mon Feb 21 2011 Pavel Derendyaev <paul@reic.ru> - 2.4.4-2
- Autostart erlyvideo

* Mon Feb 21 2011 Pavel Derendyaev <paul@reic.ru> - 2.4.4-1
- Initial build
