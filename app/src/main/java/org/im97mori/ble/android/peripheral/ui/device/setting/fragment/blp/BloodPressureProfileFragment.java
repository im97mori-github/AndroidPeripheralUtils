package org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.CheckBox;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.WorkerThread;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.card.MaterialCardView;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.ui.device.setting.DeviceSettingViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.BaseDeviceSettingFragment;
import org.im97mori.ble.android.peripheral.ui.device.setting.u180a.DeviceInformationServiceLauncherContract;
import org.im97mori.ble.android.peripheral.ui.device.setting.u1810.BloodPressureServiceLauncherContract;

import java.util.Objects;
import java.util.concurrent.TimeUnit;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class BloodPressureProfileFragment extends BaseDeviceSettingFragment {

    private BloodPressureProfileViewModel mViewModel;
    private DeviceSettingViewModel mDeviceSettingViewModel;

    private final ActivityResultLauncher<String> mStartBloodPressureServiceSettingActivity
            = registerForActivityResult(new BloodPressureServiceLauncherContract(), result -> mViewModel.setBlsDataJson(result));

    private final ActivityResultLauncher<String> mStartDeviceInformationServiceSettingActivity
            = registerForActivityResult(new DeviceInformationServiceLauncherContract(), result -> mViewModel.setDisDataJson(result));

    public BloodPressureProfileFragment() {
        super(R.layout.blood_pressure_profile_setting_fragment);
    }

    @Override
    public void onAttach(@NonNull Context context) {
        super.onAttach(context);
        mViewModel = new ViewModelProvider(requireActivity()).get(BloodPressureProfileViewModel.class);
        mDeviceSettingViewModel = new ViewModelProvider(requireActivity()).get(DeviceSettingViewModel.class);
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = Objects.requireNonNull(super.onCreateView(inflater, container, savedInstanceState));

        MaterialCardView bloodPressureServiceCardView = view.findViewById(R.id.bloodPressureServiceCardView);

        CheckBox deviceInformationServiceSupported = view.findViewById(R.id.deviceInformationServiceSupported);
        MaterialCardView deviceInformationServiceCardView = view.findViewById(R.id.deviceInformationServiceCardView);

        mViewModel.observeHasBlsData(this, bloodPressureServiceCardView::setChecked);

        mViewModel.observeIsDisSupported(this, isDisSupported -> {
            deviceInformationServiceSupported.setChecked(isDisSupported);
            deviceInformationServiceCardView.setVisibility(isDisSupported ? View.VISIBLE : View.GONE);
        });
        deviceInformationServiceSupported.setOnCheckedChangeListener((buttonView, isChecked)
                -> mViewModel.updateIsDisSupported(isChecked));
        mViewModel.observeHasDisData(this, deviceInformationServiceCardView::setChecked);

        view.findViewById(R.id.bloodPressureServiceSettingButton).setOnClickListener(v ->
                mStartBloodPressureServiceSettingActivity.launch(mViewModel.getBlsDataJson()));
        view.findViewById(R.id.deviceInformationServiceButton).setOnClickListener(v ->
                mStartDeviceInformationServiceSettingActivity.launch(mViewModel.getDisDataJson()));

        mDeviceSettingViewModel.observeMockData(requireActivity(), mockData
                -> mDisposable.add(mViewModel.setup(mockData).subscribe(() -> view.findViewById(R.id.rootContainer).setVisibility(View.VISIBLE))));
        return view;
    }

    @Nullable
    @Override
    @WorkerThread
    public String getModuleDataJson() {
        return mViewModel.getModuleDataString();
    }
}
