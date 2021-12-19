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
import org.im97mori.ble.android.peripheral.AndroidPeripheralUtilsApplication;
import org.im97mori.ble.android.peripheral.component.ApplicationComponent;
import org.im97mori.ble.android.peripheral.ui.device.setting.DeviceSettingActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.BaseDeviceSettingFragment;
import org.im97mori.ble.android.peripheral.ui.device.setting.u180a.DeviceInformationServiceLauncherContract;
import org.im97mori.ble.android.peripheral.ui.device.setting.u1810.BloodPressureServiceLauncherContract;

import java.util.Objects;

import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;

public class BloodPressureProfileFragment extends BaseDeviceSettingFragment {

    private BloodPressureProfileViewModel mViewModel;

    protected ApplicationComponent mApplicationComponent;

    private MaterialCardView mBloodPressureServiceCardView;

    private CheckBox mDeviceInformationServiceSupported;
    private MaterialCardView mDeviceInformationServiceCardView;

    final ActivityResultLauncher<String> mStartBloodPressureServiceSettingActivity = registerForActivityResult(new BloodPressureServiceLauncherContract()
            , result -> mViewModel.setBloodPressureServiceDataString(result));

    final ActivityResultLauncher<String> mStartDeviceInformationServiceSettingActivity = registerForActivityResult(new DeviceInformationServiceLauncherContract()
            , result -> mViewModel.setDeviceInformationServiceDataString(result));

    public BloodPressureProfileFragment() {
        super(R.layout.blood_pressure_profile_setting_fragment);
    }

    @Override
    public void onAttach(@NonNull Context context) {
        super.onAttach(context);
        mViewModel = new ViewModelProvider((DeviceSettingActivity) context).get(BloodPressureProfileViewModel.class);
        mApplicationComponent = ((AndroidPeripheralUtilsApplication) ((DeviceSettingActivity) context).getApplication()).getComponent();
        mApplicationComponent.inject(mViewModel);
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = Objects.requireNonNull(super.onCreateView(inflater, container, savedInstanceState));

        mBloodPressureServiceCardView = view.findViewById(R.id.bloodPressureServiceCardView);

        mDeviceInformationServiceSupported = view.findViewById(R.id.deviceInformationServiceSupported);
        mDeviceInformationServiceCardView = view.findViewById(R.id.deviceInformationServiceCardView);

        mViewModel.observeHasBls(this, aBoolean -> mBloodPressureServiceCardView.setChecked(aBoolean));
        mViewModel.observeSupportDis(this, aBoolean -> {
            if (aBoolean) {
                mDeviceInformationServiceCardView.setVisibility(View.VISIBLE);
            } else {
                mDeviceInformationServiceCardView.setChecked(false);
                mDeviceInformationServiceCardView.setVisibility(View.GONE);
            }
            mDeviceInformationServiceSupported.setChecked(aBoolean);
        });
        mDeviceInformationServiceSupported.setOnCheckedChangeListener((buttonView, isChecked)
                -> mViewModel.updateSupportDis(isChecked));
        mViewModel.observeHasDis(this, aBoolean -> mDeviceInformationServiceCardView.setChecked(aBoolean));

        view.findViewById(R.id.bloodPressureServiceSettingButton).setOnClickListener(v ->
                mStartBloodPressureServiceSettingActivity.launch(mViewModel.getBloodPressureServiceDataString()));
        view.findViewById(R.id.deviceInformationServiceButton).setOnClickListener(v ->
                mStartDeviceInformationServiceSettingActivity.launch(mViewModel.getDeviceInformationServiceDataString()));

        return view;
    }

    @Override
    public void onStart() {
        super.onStart();
        mDisposable.add(mViewModel.setup(requireArguments())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(() -> requireView().findViewById(R.id.rootContainer).setVisibility(View.VISIBLE)));
    }

    @Nullable
    @Override
    @WorkerThread
    public String getModuleDataString() {
        return mViewModel.getModuleDataString();
    }
}
