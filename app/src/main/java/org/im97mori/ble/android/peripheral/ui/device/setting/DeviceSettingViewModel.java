package org.im97mori.ble.android.peripheral.ui.device.setting;

import android.content.Intent;
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

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;

import org.im97mori.ble.MockData;
import org.im97mori.ble.android.peripheral.Constants;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceRepository;
import org.im97mori.ble.android.peripheral.room.Device;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.BaseDeviceSettingFragment;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp.BloodPressureProfileFragment;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class DeviceSettingViewModel extends ViewModel {

    private static final String KEY_TITLE = "KEY_TITLE";
    private static final String KEY_DEVICE_SETTING_NAME = "KEY_DEVICE_SETTING_NAME";
    private static final String KEY_DEVICE_SETTING_NAME_ERROR_STRING = "KEY_DEVICE_SETTING_NAME_ERROR_STRING";
    private static final String KEY_MOCK_DATA = "KEY_MOCK_DATA";

    private final DeviceRepository mDeviceRepository;
    private final SavedStateHandle mSavedStateHandle;
    private final Gson mGson;

    private Device mDevice;

    private BaseDeviceSettingFragment mBaseDeviceSettingFragment;

    @Inject
    public DeviceSettingViewModel(@NonNull SavedStateHandle savedStateHandle, @NonNull DeviceRepository deviceRepository, @NonNull Gson gson) {
        mSavedStateHandle = savedStateHandle;
        mDeviceRepository = deviceRepository;
        mGson = gson;
    }

    @NonNull
    @MainThread
    public Completable setup(@NonNull Intent intent) {
        long id = intent.getLongExtra(DeviceSettingLauncherContract.KEY_DEVICE_ID, DeviceSettingLauncherContract.UNSAVED_DEVICE_ID);
        Single<Device> single;
        if (DeviceSettingLauncherContract.UNSAVED_DEVICE_ID == id) {
            single = Single.just(new Device("", intent.getIntExtra(DeviceSettingLauncherContract.KEY_DEVICE_TYPE, Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE)));
        } else {
            single = mDeviceRepository.loadDeviceById(id).subscribeOn(Schedulers.io());
        }
        return single
                .flatMapCompletable(device -> {
                    // for 1st onStart
                    if (mDevice == null) {
                        mDevice = device;

                        // for Activity create
                        MutableLiveData<String> liveData = mSavedStateHandle.getLiveData(KEY_TITLE);
                        if (liveData.getValue() == null) {
                            liveData.postValue(mDeviceRepository.getDeviceTypeName(mDevice.getDeviceType()));
                            mSavedStateHandle.getLiveData(KEY_DEVICE_SETTING_NAME).postValue(mDevice.getDeviceSettingName());
                            mSavedStateHandle.getLiveData(KEY_DEVICE_SETTING_NAME_ERROR_STRING)
                                    .postValue(mDeviceRepository.getDeviceSettingNameErrorString(mDevice.getDeviceSettingName()));

                            MockData mockData = null;
                            try {
                                mockData = mGson.fromJson(mDevice.getDeviceSetting(), MockData.class);
                            } catch (JsonSyntaxException e) {
                                e.printStackTrace();
                            } finally {
                                if (mockData == null) {
                                    mockData = new MockData();
                                }
                            }
                            mSavedStateHandle.getLiveData(KEY_MOCK_DATA).postValue(mockData);
                        }
                        return Completable.complete();
                    } else {
                        throw new RuntimeException("Initialized");
                    }
                })
                .observeOn(AndroidSchedulers.mainThread());
    }

    @MainThread
    public void observeTitle(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_TITLE)).observe(owner, observer);
    }

    @MainThread
    public void observeDeviceSettingName(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_DEVICE_SETTING_NAME)).observe(owner, observer);
    }

    @MainThread
    public void observeDeviceSettingNameErrorString(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_DEVICE_SETTING_NAME_ERROR_STRING)).observe(owner, observer);
    }

    @MainThread
    public void observeMockData(@NonNull LifecycleOwner owner, @NonNull Observer<MockData> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<MockData>getLiveData(KEY_MOCK_DATA)).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateDeviceSettingName(@Nullable String text) {
        mSavedStateHandle.<String>getLiveData(KEY_DEVICE_SETTING_NAME).setValue(text);
        mSavedStateHandle.getLiveData(KEY_DEVICE_SETTING_NAME_ERROR_STRING)
                .postValue(mDeviceRepository.getDeviceSettingNameErrorString(text));
    }

    @NonNull
    @MainThread
    public BaseDeviceSettingFragment getFragment(@NonNull Intent intent) {
        mBaseDeviceSettingFragment = new BloodPressureProfileFragment();
        return mBaseDeviceSettingFragment;
    }

    @NonNull
    @MainThread
    public Completable save() {
        return Single.<Device>create(emitter -> {
            Device device = mDevice;
            if (device == null) {
                emitter.onError(new RuntimeException("Already saved"));
            } else {
                String deviceNameSetting = mSavedStateHandle.get(KEY_DEVICE_SETTING_NAME);
                String deviceNameSettingErrorString = mSavedStateHandle.get(KEY_DEVICE_SETTING_NAME_ERROR_STRING);
                String moduleDataJson = mBaseDeviceSettingFragment.getModuleDataJson();
                if (!TextUtils.isEmpty(deviceNameSetting)
                        && TextUtils.isEmpty(deviceNameSettingErrorString)
                        && !TextUtils.isEmpty(moduleDataJson)) {
                    device.setDeviceSettingName(deviceNameSetting);
                    device.setDeviceSetting(moduleDataJson);
                    mDevice = null;
                    emitter.onSuccess(device);
                } else {
                    emitter.onError(new RuntimeException("Validation failed"));
                }
            }
        }).subscribeOn(Schedulers.io())
                .flatMapCompletable(device -> {
                    mDeviceRepository.insertDevices(device);
                    return Completable.complete();
                })
                .observeOn(AndroidSchedulers.mainThread());
    }

}