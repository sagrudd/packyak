%global packname zoo
%global pyversion 3.8
%global packrel 1
%global debug_package %{nil}
%global _python_bytecompile_errors_terminate_build 0

Name:             symbioinfo-python-six
Version:          1.8.8
Release:          %{packrel}%{?dist}
Source0:          https://cran.r-project.org/src/contrib/%{packname}_1.8-8.tar.gz
License:          GPL-2 | GPL-3
URL:              http://cran.rstudio.com/web/packages/zoo/index.html
Group:            Applications/Bioinformatics
Summary:          Automated Build of package = zoo (1.8.8)

%global _description %{expand:
This workflow has been prepared by the PackYak and description parsing has not
yet been implemented - this is a TODO
}

%description %_description

%package -n python3-%{packname}
%{?python_provide:%python_provide python3-%{packname}}

Summary:        %{summary}
BuildRequires:    python-devel
Requires:         python-devel

%description -n python3-%{packname} %_description

%prep
%autosetup -p1 -n %{packname}-%{version}
pathfix.py -pni "%{__python3} %{py3_shbang_opts}" .

%build
CFLAGS="${CFLAGS:-${RPM_OPT_FLAGS}}" LDFLAGS="${LDFLAGS:-${RPM_LD_FLAGS}}"\
  /usr/bin/python%{pyversion} setup.py  build --executable="/usr/bin/python%{pyversion} -s"

%install
CFLAGS="${CFLAGS:-${RPM_OPT_FLAGS}}" LDFLAGS="${LDFLAGS:-${RPM_LD_FLAGS}}"\
  /usr/bin/python%{pyversion} setup.py  install -O1 --skip-build --root %{buildroot}
if ( [ -d %{buildroot}%{_bindir} ] ); then
    pathfix.py -pni "%{__python3} %{py3_shbang_opts}" %{buildroot}/usr/lib/python%{pyversion}/site-packages/ %{buildroot}%{_bindir}/*
fi

%check

%clean
rm -rf $RPM_BUILD_ROOT
rm -fR %{_builddir}/%{packname}*

%files -n python3-pandas
/usr/lib/python%{pyversion}/site-packages/%{packname}*
/usr/bin/*

%changelog
* Mon Feb 1 2021 sagrudd <stephen@mnemosyne.co.uk>
- updated the R template for usage in Python deployments
- somewhat adherent to https://docs.fedoraproject.org/en-US/packaging-guidelines/Python/
* Mon Jan 11 2021 Stephen Rudd <stephen@mnemosyne.co.uk>
- resurrecting the symbioinfo concept for arm64 usage
* Tue Jan 31 2017 Stephen Rudd <stephen@mnemosyne.co.uk>
- This is a first version of the R package specfile for SymBioInfo
- this is based on docs from https://fedoraproject.org/wiki/Packaging:R?rd=Packaging/R
- this is also intended as a template for automation ...
