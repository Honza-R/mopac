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

      subroutine mopend(txt)
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      USE molkst_C, only : moperr, errtxt
      use chanel_C, only : iw
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      character , intent(in) :: txt*(*)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
!-----------------------------------------------
      moperr = .TRUE.
      errtxt = txt
      call summary(txt, len_trim(txt))
      if (trim(txt) /= "JOB ENDED NORMALLY" ) write(iw,'(/10x,a)')trim(txt)
      call to_screen("To_file:END_OF_JOB"//trim(txt))
      return
      end subroutine mopend
      subroutine summary(txt, ntxt)
!
!  Collect error messages and print them at the end of each calculation
!
!
        use chanel_C, only : iw, ir
        use molkst_C, only : line, job_no, job_no0, natoms, dummy, errtxt
        implicit none
        integer, intent (in) :: ntxt
        character, intent (in) :: txt*(*)
!
!  Local quantities
!
        integer, parameter :: lim = 120, hook = 50
        character :: messages(20)*(lim), blank*(lim)
        integer :: nmessages = 0, i, j, max_txt
        logical :: first = .true., opend
        save
! vvv MOPAC API hijacking of the error handler vvv
        ! report number of errors in dummy
        if (ntxt == 0) then
          dummy = nmessages
          return
        ! record error message in errtxt
        else if (ntxt < 0) then
          ! reset error handler
          if (ntxt < -nmessages) then
            first = .true.
            nmessages = 0
            return
          end if
          errtxt = trim(messages(-ntxt))
          return
        end if
! ^^^ MOPAC API hijacking of the error handler ^^^
        if (first) then
           messages(1)(:18) = "JOB ENDED NORMALLY"
           first = .false.
!
!   Add closing message to AUX file
!
           inquire(unit=hook, opened=opend)
           if (opend) then
             write(hook,"(a)")" END OF MOPAC PROGRAM"
           end if
        end if
        if (ntxt == 1) then
          if (natoms == 0 .and. job_no == job_no0) then
            write(iw,'(/10x, a)')"Job failed to run because no atoms were detected in the system"
            write(iw,'(10x, a, /)')"The start of the data-set is as follows:"
            rewind (ir)
            max_txt = 0
            do i = 1, 9
              read(ir, '(a)', iostat = j)line
              if (j /= 0) exit
              max_txt = max(max_txt, len_trim(line))
            end do
            rewind (ir)
            do i = 1, 9
              read(ir, '(a)', iostat = j)line
              if (j /= 0) exit
              write(iw,'(a, i1, a)')" Line ", i, ": """//line(:max_txt)//""""
            end do
            if (i < 10) write(iw,'(/,a)')"          Then the end of the data-set was detected"
          end if
!
!   Write out messages
!
          blank = " "
          blank(1:1) = "*"
          max_txt = 1
          do i = 1, nmessages
            max_txt = max(max_txt, len_trim(messages(i)))
          end do
          max_txt = min(lim, max(max_txt + 4, 22))
          if (messages(1)(1:18) /= "JOB ENDED NORMALLY" ) max_txt = max(max_txt, 78)
          write(iw,'(/1x,120a)')("*", i = 1, max_txt)
          blank(max_txt:max_txt) = "*"
          write(iw,'(1x,a)')trim(blank)
          if (messages(1)(1:18) /= "JOB ENDED NORMALLY" ) then
            write(line,'(a)') "*     Error and normal termination messages reported in this calculation"
            line(max_txt:max_txt) = "*"
            write(iw,'(1x,a)')trim(line)
            write(iw,'(1x,a)')trim(blank)
            do i = 1, nmessages
              if (index(messages(i),"JOB ENDED NORMALLY") > 0) cycle
              write(line,'(a)')"* "//trim(messages(i))
              line(max_txt:max_txt) = "*"
              write(iw,'(1x,a)')trim(line)
            end do
          end if
          write(line,'(a)') "* JOB ENDED NORMALLY "
          line(max_txt:max_txt) = "*"
          write(iw,'(1x,a)')trim(line)
          write(iw,'(1x,a)')trim(blank)
          write(iw,'(1x,120a)')("*", i = 1, max_txt)
          nmessages = 0
          messages(1) = "JOB ENDED NORMALLY"
        else
!
!   Store current message
!
          if (nmessages == 20) return
          nmessages = nmessages + 1
          i = min(ntxt, lim - 3)
          messages(nmessages) = txt(:i)
        end if
        return
      end subroutine summary
