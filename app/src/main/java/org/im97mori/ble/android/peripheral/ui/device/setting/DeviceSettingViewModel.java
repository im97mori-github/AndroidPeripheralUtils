package org.im97mori.ble.android.peripheral.ui.device.setting;

import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;
import androidx.lifecycle.SavedStateHandle;
import androidx.lifecycle.Transformations;
import androidx.lifecycle.ViewModel;

import org.im97mori.ble.android.peripheral.Constants;
import org.im97mori.ble.android.peripheral.datasource.DeviceDataSource;
import org.im97mori.ble.android.peripheral.datasource.ResourceTextSource;
import org.im97mori.ble.android.peripheral.persistence.Device;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.BaseDeviceSettingFragment;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp.BloodPressureProfileFragment;

import java.util.Objects;

import javax.inject.Inject;

import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.schedulers.Schedulers;

public class DeviceSettingViewModel extends ViewModel {

    @Inject
    DeviceDataSource mDeviceDataSource;

    @Inject
    ResourceTextSource mResourceTextSource;

    private Device mDevice;

    private BaseDeviceSettingFragment mBaseDeviceSettingFragment;

    private final MutableLiveData<String> title;
    private final MutableLiveData<String> deviceSettingNameError;
    private final MutableLiveData<String> deviceSettingNameEdit;

    public DeviceSettingViewModel(@NonNull SavedStateHandle savedStateHandle) {
        title = savedStateHandle.getLiveData("title");
        deviceSettingNameError = savedStateHandle.getLiveData("deviceSettingNameError");
        deviceSettingNameEdit = savedStateHandle.getLiveData("deviceSettingNameEdit");
    }

    @NonNull
    @MainThread
    public Completable setup(@NonNull Intent intent) {
        long id = intent.getLongExtra(DeviceSettingLauncherContract.KEY_DEVICE_ID, DeviceSettingLauncherContract.UNSAVED_DEVICE_ID);
        Single<Device> single;
        if (DeviceSettingLauncherContract.UNSAVED_DEVICE_ID == id) {
            single = Single.just(new Device("", intent.getIntExtra(DeviceSettingLauncherContract.KEY_DEVICE_TYPE, Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE)));
        } else {
            single = mDeviceDataSource.loadDeviceById(id).subscribeOn(Schedulers.io());
        }
        return single.observeOn(Schedulers.trampoline())
                .flatMapCompletable(device -> {
                    // for 1st onStart
                    if (mDevice == null) {
                        mDevice = device;

                        // for Activity create
                        if (title.getValue() == null) {
                            title.postValue(mDeviceDataSource.getDeviceTypeName(mDevice.getDeviceType()));
                            deviceSettingNameEdit.postValue(mDevice.getDeviceName());
                            deviceSettingNameError.postValue(mResourceTextSource.getDeviceSettingNameErrorString(mDevice.getDeviceName()));
                        }
                        return Completable.complete();
                    } else {
                        throw new RuntimeException("Initialized");
                    }
                })
                .observeOn(AndroidSchedulers.mainThread());
    }

    public void observeTitle(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(title).observe(owner, observer);
    }

    public void observeDeviceNameError(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(deviceSettingNameError).observe(owner, observer);
    }

    public void observeDeviceNameEdit(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(deviceSettingNameEdit).observe(owner, observer);
    }

    public synchronized void updateDeviceNameEdit(@Nullable String text) {
        deviceSettingNameEdit.setValue(text);
        deviceSettingNameError.setValue(mResourceTextSource.getDeviceSettingNameErrorString(text));
    }

    @NonNull
    public BaseDeviceSettingFragment getFragment(@NonNull Intent intent) {
        mBaseDeviceSettingFragment = new BloodPressureProfileFragment();
        Bundle bundle = new Bundle();
        bundle.putLong(DeviceSettingLauncherContract.KEY_DEVICE_ID, intent.getLongExtra(DeviceSettingLauncherContract.KEY_DEVICE_ID, DeviceSettingLauncherContract.UNSAVED_DEVICE_ID));
        mBaseDeviceSettingFragment.setArguments(bundle);
        return mBaseDeviceSettingFragment;
    }

    @NonNull
    public Completable save() {
        return Single.<Device>create(emitter -> {
            Device device = mDevice;
            if (device == null) {
                throw new RuntimeException("Already saved");
            } else {
                String moduleDataString = mBaseDeviceSettingFragment.getModuleDataString();
                if (deviceSettingNameError.getValue() == null
                        && !TextUtils.isEmpty(moduleDataString)) {
                    device.setDeviceName(Objects.requireNonNull(deviceSettingNameEdit.getValue()));
                    device.setDeviceSetting(moduleDataString);
                    mDevice = null;
                    emitter.onSuccess(device);
                } else {
                    throw new RuntimeException("Validation failed");
                }
            }
        }).subscribeOn(Schedulers.io())
                .flatMapCompletable(device -> {
                    mDeviceDataSource.insertDevices(device);
                    return Completable.complete();
                })
                .observeOn(AndroidSchedulers.mainThread());
    }

}