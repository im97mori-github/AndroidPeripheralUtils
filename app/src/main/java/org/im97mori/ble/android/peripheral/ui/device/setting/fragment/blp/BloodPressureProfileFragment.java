package org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.WorkerThread;
import androidx.lifecycle.ViewModelProvider;

import org.im97mori.ble.android.peripheral.databinding.BloodPressureProfileSettingFragmentBinding;
import org.im97mori.ble.android.peripheral.ui.device.setting.DeviceSettingViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.BaseDeviceSettingFragment;
import org.im97mori.ble.android.peripheral.ui.device.setting.u180a.DeviceInformationServiceLauncherContract;
import org.im97mori.ble.android.peripheral.ui.device.setting.u1810.BloodPressureServiceLauncherContract;
import org.im97mori.stacklog.LogUtils;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class BloodPressureProfileFragment extends BaseDeviceSettingFragment {

    private BloodPressureProfileViewModel mViewModel;
    private DeviceSettingViewModel mDeviceSettingViewModel;

    private final ActivityResultLauncher<String> mStartBloodPressureServiceSettingActivity
            = registerForActivityResult(new BloodPressureServiceLauncherContract(), result -> mViewModel.setBlsDataJson(result));

    private final ActivityResultLauncher<String> mStartDeviceInformationServiceSettingActivity
            = registerForActivityResult(new DeviceInformationServiceLauncherContract(), result -> mViewModel.setDisDataJson(result));

    @Override
    public void onAttach(@NonNull Context context) {
        super.onAttach(context);
        mViewModel = new ViewModelProvider(requireActivity()).get(BloodPressureProfileViewModel.class);
        mDeviceSettingViewModel = new ViewModelProvider(requireActivity()).get(DeviceSettingViewModel.class);
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        BloodPressureProfileSettingFragmentBinding binding = BloodPressureProfileSettingFragmentBinding.inflate(inflater, container, false);

        mViewModel.observeHasBlsDataJson(this, binding.bloodPressureServiceCardView::setChecked);

        mViewModel.observeIsDisSupported(this, isDisSupported -> {
            binding.isDeviceInformationServiceSupported.setChecked(isDisSupported);
            binding.deviceInformationServiceCardView.setVisibility(isDisSupported ? View.VISIBLE : View.GONE);
        });
        binding.isDeviceInformationServiceSupported.setOnCheckedChangeListener((buttonView, isChecked)
                -> mViewModel.updateIsDisSupported(isChecked));
        mViewModel.observeHasDisDataJson(this, binding.deviceInformationServiceCardView::setChecked);

        binding.bloodPressureServiceSettingButton.setOnClickListener(v ->
                mStartBloodPressureServiceSettingActivity.launch(mViewModel.getBlsDataJson()));

        binding.deviceInformationServiceButton.setOnClickListener(v ->
                mStartDeviceInformationServiceSettingActivity.launch(mViewModel.getDisDataJson()));

        mDisposable.add(mDeviceSettingViewModel.observeMockData()
                .subscribe(mockData -> mDisposable.add(mViewModel.setup(mockData)
                                .subscribe(() -> mDeviceSettingViewModel.fragmentReady(), throwable -> LogUtils.stackLog(throwable.getMessage())))
                        , throwable -> LogUtils.stackLog(throwable.getMessage())));
        return binding.getRoot();
    }

    @Nullable
    @Override
    @WorkerThread
    public String getModuleDataJson() {
        return mViewModel.getModuleDataString();
    }

}
