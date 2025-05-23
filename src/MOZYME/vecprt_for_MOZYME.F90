! Molecular Orbital PACkage (MOPAC)
! Copyright 2021 Virginia Polytechnic Institute and State University
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!    http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

subroutine vecprt_for_MOZYME(aa, numm)
    use molkst_C, only: mpack, numat
    use chanel_C, only: iw
    use elemts_C, only: elemnt
    use common_arrays_C, only : nat, nfirst, nlast, l_atom
    implicit none
    double precision, dimension (mpack), intent (in) :: aa
    integer, intent (in) :: numm
    integer, parameter :: maxarr = 200
!
    integer :: i, ii, ij, j, jhi, jj, jlo, k, kk, l, limit, linear, ll, m, ma, &
   & n, na, numb
    double precision :: fact, sumax
    character (len=2), dimension (9), save :: atorbs
    character (len=2), dimension (maxarr) :: itext, jtext
    character (len=6), dimension (21) :: line
    integer, dimension (maxarr) :: natom
    double precision, dimension (:), allocatable :: a
    integer, external :: ijbo
    data atorbs / " S", "PX", "PY", "PZ", "X2", "XZ", "Z2", "YZ", "XY" /
!
    l = 0
    ll = 0
    numb = Abs (numm)
    if (numb > maxarr) then
      write (iw,'(/10x,a,i5)') "VECPRT CAN ONLY PRINT ARRAYS OF SIZE LESS THAN", maxarr
      write (iw,'(10x,a,i5)') "AN ATTEMPT WAS MADE TO PRINT AN ARRAY OF SIZE ", numb
      numb = maxarr
    end if
    linear = (numb*(numb+1)) / 2
    allocate (a(linear))
   !
   !    Decide what type of array to print.  Options are:
   !        (1)   Over atoms
   !        (2)   Over atomic orbitals
   !        (3)   All other types.
   !
    if (numat /= 0 .and. numat == numm) then
      !
      !    OPTION (1):  PRINT OVER ATOM COUNT
      !
      j = 0
      l = 0
      do i = 1, numb
        if (l_atom(i)) then
          j = j + 1
          itext(j) = "  "
          jtext(j) = elemnt(nat(i))
          natom(j) = i
          do k = 1, i
            if (l_atom(k)) then
              l = l + 1
              a(l) = aa((i*(i - 1))/2 + k)
            end if
          end do
        end if
      end do
      numb = j
      linear = l
    else if (numat /= 0 .and. nlast(numat) == numm) then
      !
      !   OPTION (2): PRINT OVER ATOMIC ORBITALS.  Before
      !   printing, the array has to be put into standard form.
      !
      do i = 1, linear
        a(i) = 0.d0
      end do
      ij = 0
      do i = 1, numat
        do j = 1, i - 1
          if (ijbo(i, j) >= 0) then
            ij = ij + 1
            l = ijbo (i, j)
            do ii = nfirst(i), nlast(i)
              do jj = nfirst(j), nlast(j)
                l = l + 1
                ll = (ii*(ii-1)) / 2 + jj
                if (ll <= linear) a(ll) = aa(l)
              end do
            end do
          end if
        end do
        ij = ij + 1
        l = ijbo (i, i)
        do ii = nfirst(i), nlast(i)
          do jj = nfirst(i), ii
            l = l + 1
            ll = (ii*(ii-1)) / 2 + jj
            if (ll <= linear)a(ll) = aa(l)
          end do
        end do
        if (ll == linear) exit
      end do
      outer_loop: do i = 1, numat
        jlo = nfirst(i)
        jhi = nlast(i)
        l = nat(i)
        k = 0
        do j = jlo, jhi
          k = k + 1
          itext(j) = atorbs(k)
          jtext(j) = elemnt(l)
          natom(j) = i
          if (j == numb) exit outer_loop
        end do
      end do outer_loop
    else
      !
      !    OPTION (3):  PRINT A GENERIC ARRAY.
      !
      do i = 1, numb
        itext (i) = "  "
        jtext (i) = "  "
        natom(i) = i
      end do
      do i = 1, linear
        a(i) = aa(i)
      end do
    end if
   !
   !   Scale diagonal terms so that they will always be printable.
   !
    sumax = 1.d0
    do i = 1, numb
      sumax = Max (Abs (a((i*(i+1))/2)), sumax)
    end do
    i = Int (Log10(sumax))
    if (i == 1 .or. i == 2) then
      i = 0
    end if
    fact = 10.d0 ** (-i)
    if (Abs (fact-1.d0) > 0.001d0) then
      write (iw, "(/10x,A,F16.6)") "Diagonal Terms should be Multiplied by", 1.d0 / &
     & fact
      do i = 1, numb
        a((i*(i+1))/2) = a((i*(i+1))/2) * fact
      end do
    end if
    do i = 1, 21
      line (i) = "------"
    end do
    limit = (numb*(numb+1)) / 2
    kk = 8
    na = 1
    do
      ll = 0
      m = Min ((numb+1-na), 6)
      ma = 2 * m + 1
      m = na + m - 1
      !
10000 format (/,/,13x,10(1x,a2,1x,a2,i3,2x))
      write (iw, 10000) (itext(i), jtext(i), natom(i), i=na, m)
10010 format (" ", 21 a6)
      write (iw, 10010) (line(k), k=1, ma)
      do i = na, numb
        ll = ll + 1
        k = (i*(i-1)) / 2
        l = Min ((k+m), (k+i))
        k = k + na
        if ((kk+ll) > 50) then
          write (iw, 10000) (itext(n), jtext(n), natom(n), n=na, m)
          write (iw, 10010) (line(n), n=1, ma)
          kk = 4
          ll = 0
        end if
10030   format (' ',a2,1x,a2,i5,10f11.6)
        write (iw, 10030) itext(i), jtext(i), natom(i), (a(n), n=k, l)
      end do
      if (l >= limit) exit
      kk = kk + ll + 4
      na = m + 1
      if ((kk+numb+1-na) > 50) then
        kk = 4
      end if
    end do
    deallocate (a)
    return
!
end subroutine vecprt_for_MOZYME
