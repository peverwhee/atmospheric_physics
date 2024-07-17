module cu_ntiedtke_pre_sima

 use ccpp_kinds,only: kind_phys

 implicit none
 private
 public:: cu_ntiedtke_pre_sima_init
 public:: cu_ntiedtke_pre_sima_run


 contains

!=================================================================================================================
!>\section arg_table_cu_ntiedtke_pre_sima_init
!!\html\include cu_ntiedtke_pre_sima_init.html
!!
 subroutine cu_ntiedtke_pre_sima_init(qvften, thften, height, thickness, &
                 slimsk, evap, hfx, cell_size, stepcu, errmsg, errflg)
!=================================================================================================================

!--- input arguments:
 real(kind=kind_phys), intent(in)  :: height(:,:)

!--- output arguments:
 real(kind=kind_phys), intent(out) :: qvften(:,:)
 real(kind=kind_phys), intent(out) :: thften(:,:)
 real(kind=kind_phys), intent(out) :: thickness(:,:)
 integer,              intent(out) :: slimsk(:)
 real(kind=kind_phys), intent(out) :: evap(:)
 real(kind=kind_phys), intent(out) :: hfx(:)
 real(kind=kind_phys), intent(out) :: cell_size(:)
 integer,              intent(out) :: stepcu
 character(len=*),     intent(out) :: errmsg     ! output error message (-).
 integer,              intent(out) :: errflg     ! output error flag (-).

 ! Local variables
 integer :: idx, kdx

!-----------------------------------------------------------------------------------------------------------------

!--- output error flag and message:
 errflg = 0
 errmsg = " "

 qvften = 0._kind_phys
 thften = 0._kind_phys
 hfx = 0._kind_phys
 evap = 0._kind_phys
 cell_size = 10000._kind_phys
 slimsk = 1
 stepcu = 1
 do idx = 1, size(thickness, 1)
   do kdx = 1, size(thickness, 2)
      thickness(idx, kdx) = height(idx, kdx+1) - height(idx, kdx)
   end do
 end do

 end subroutine cu_ntiedtke_pre_sima_init
 
!=================================================================================================================
!>\section arg_table_cu_ntiedtke_pre_sima_run
!!\html\include cu_ntiedtke_pre_sima_run.html
!!
 subroutine cu_ntiedtke_pre_sima_run(errmsg, errflg)
 character(len=*),     intent(out) :: errmsg     ! output error message (-).
 integer,              intent(out) :: errflg     ! output error flag (-).

!-----------------------------------------------------------------------------------------------------------------

!--- output error flag and message:
 errflg = 0
 errmsg = " "
 end subroutine cu_ntiedtke_pre_sima_run
end module cu_ntiedtke_pre_sima
