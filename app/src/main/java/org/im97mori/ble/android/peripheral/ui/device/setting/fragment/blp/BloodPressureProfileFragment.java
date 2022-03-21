package org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import org.im97mori.ble.android.peripheral.databinding.BloodPressureProfileSettingFragmentBinding;
import org.im97mori.ble.android.peripheral.ui.device.setting.DeviceSettingViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.u180a.DeviceInformationServiceLauncherContract;
import org.im97mori.ble.android.peripheral.ui.device.setting.u1810.BloodPressureServiceLauncherContract;
import org.im97mori.ble.android.peripheral.utils.AutoDisposeViewModelProvider;
import org.im97mori.stacklog.LogUtils;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class BloodPressureProfileFragment extends Fragment {

    private BloodPressureProfileViewModel mViewModel;
    private DeviceSettingViewModel mDeviceSettingViewModel;

    private final ActivityResultLauncher<byte[]> mStartBloodPressureServiceSettingActivity
            = registerForActivityResult(new BloodPressureServiceLauncherContract(), result -> mViewModel.setBlsData(result));

    private final ActivityResultLauncher<byte[]> mStartDeviceInformationServiceSettingActivity
            = registerForActivityResult(new DeviceInformationServiceLauncherContract(), result -> mViewModel.setDisData(result));

    @Override
    public void onAttach(@NonNull Context context) {
        super.onAttach(context);
        AutoDisposeViewModelProvider viewModelProvider = new AutoDisposeViewModelProvider(requireActivity());
        mViewModel = viewModelProvider.get(BloodPressureProfileViewModel.class);
        mDeviceSettingViewModel = viewModelProvider.get(DeviceSettingViewModel.class);
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        BloodPressureProfileSettingFragmentBinding binding = BloodPressureProfileSettingFragmentBinding.inflate(inflater, container, false);

        mViewModel.observeHasBlsData(this, binding.bloodPressureServiceCardView::setChecked);

        mViewModel.observeIsDisSupported(this, isDisSupported -> {
            binding.isDeviceInformationServiceSupported.setChecked(isDisSupported);
            binding.deviceInformationServiceCardView.setVisibility(isDisSupported ? View.VISIBLE : View.GONE);
        });
        binding.isDeviceInformationServiceSupported.setOnCheckedChangeListener((buttonView, isChecked)
                -> mViewModel.updateIsDisSupported(isChecked));
        mViewModel.observeHasDisData(this, binding.deviceInformationServiceCardView::setChecked);

        binding.bloodPressureServiceSettingButton.setOnClickListener(v ->
                mStartBloodPressureServiceSettingActivity.launch(mViewModel.getBlsData()));

        binding.deviceInformationServiceSettingButton.setOnClickListener(v ->
                mStartDeviceInformationServiceSettingActivity.launch(mViewModel.getDisData()));

        mDeviceSettingViewModel.observeMockData(this
                , data -> mViewModel.observeSetup(data
                        , () -> mDeviceSettingViewModel.fragmentReady()
                        , throwable -> LogUtils.stackLog(throwable.getMessage())));
        return binding.getRoot();
    }

}
