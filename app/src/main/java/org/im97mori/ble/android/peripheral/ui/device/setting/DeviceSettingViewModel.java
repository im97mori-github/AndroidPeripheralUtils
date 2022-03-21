package org.im97mori.ble.android.peripheral.ui.device.setting;

import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE;
import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_UNDEFINED;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_ID;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_TYPE;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.VALUE_DEVICE_ID_UNSAVED;

import android.content.Intent;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;
import androidx.lifecycle.SavedStateHandle;
import androidx.lifecycle.Transformations;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.BaseSettingFragmentViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp.BloodPressureProfileFragment;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp.BloodPressureProfileViewModel;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class DeviceSettingViewModel extends BaseSettingViewModel<Intent, Boolean> {

    private static final String KEY_DEVICE_TYPE_IMAGE_RES_ID = "KEY_DEVICE_TYPE_IMAGE_RES_ID";
    private static final String KEY_DEVICE_TYPE_NAME = "KEY_DEVICE_TYPE_NAME";
    private static final String KEY_DEVICE_SETTING_NAME = "KEY_DEVICE_SETTING_NAME";
    private static final String KEY_FRAGMENT_READY = "KEY_FRAGMENT_READY";
    private static final String KEY_MOCK_DATA = "KEY_MOCK_DATA";

    private final SavedStateHandle mSavedStateHandle;

    private final MutableLiveData<String> mDeviceNameSetting;
    private final MutableLiveData<Boolean> mFragmentReady;

    private final MutableLiveData<byte[]> mMockData;

    private final MutableLiveData<Boolean> mSavedData;

    private DeviceSetting mDeviceSetting;

    @Inject
    public DeviceSettingViewModel(@NonNull SavedStateHandle savedStateHandle
            , @NonNull DeviceSettingRepository deviceSettingRepository) {
        super(deviceSettingRepository);

        mSavedStateHandle = savedStateHandle;

        mDeviceNameSetting = savedStateHandle.getLiveData(KEY_DEVICE_SETTING_NAME);
        mFragmentReady = savedStateHandle.getLiveData(KEY_FRAGMENT_READY);

        mMockData = savedStateHandle.getLiveData(KEY_MOCK_DATA);

        mSavedData = savedStateHandle.getLiveData(KEY_SAVED_DATA);
    }

    @MainThread
    public void observeSetup(@NonNull Intent intent, @NonNull Action onComplete, @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Single.just(intent.getLongExtra(KEY_DEVICE_ID, VALUE_DEVICE_ID_UNSAVED))
                .flatMap(id -> {
                    if (VALUE_DEVICE_ID_UNSAVED == id) {
                        return Single.just(new DeviceSetting(intent.getIntExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_UNDEFINED)));
                    } else {
                        return mDeviceSettingRepository.loadDeviceSettingById(id);
                    }
                })
                .subscribeOn(Schedulers.io())
                .flatMapCompletable(device -> {
                    // for 1st onStart
                    mDeviceSetting = device;

                    MutableLiveData<String> liveData = mSavedStateHandle.getLiveData(KEY_DEVICE_TYPE_NAME);
                    if (liveData.getValue() == null) {
                        liveData.postValue(mDeviceSettingRepository.getDeviceTypeName(mDeviceSetting.getDeviceType()));
                        mSavedStateHandle.<Integer>getLiveData(KEY_DEVICE_TYPE_IMAGE_RES_ID)
                                .postValue(mDeviceSettingRepository.getDeviceTypeImageResId(mDeviceSetting.getDeviceType()));
                        mDeviceNameSetting.postValue(mDeviceSetting.getDeviceSettingName());
                    }

                    mMockData.postValue(mDeviceSetting.getDeviceSettingData());

                    return Completable.complete();

                })
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(onComplete, onError));
    }

    @MainThread
    public void observeDeviceTypeImageResId(@NonNull LifecycleOwner owner, @NonNull Observer<Integer> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<Integer>getLiveData(KEY_DEVICE_TYPE_IMAGE_RES_ID)).observe(owner, observer);
    }

    @MainThread
    public void observeDeviceTypeName(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_DEVICE_TYPE_NAME)).observe(owner, observer);
    }

    @MainThread
    public void observeDeviceSettingName(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mDeviceNameSetting).observe(owner, observer);
    }

    @MainThread
    public void observeDeviceSettingNameErrorString(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mDeviceNameSetting).observe(owner
                , s -> observer.onChanged(mDeviceSettingRepository.getDeviceSettingNameErrorString(s)));
    }

    @MainThread
    public void observeFragmentReady(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mFragmentReady).observe(owner, observer);
    }

    public void observeMockData(@NonNull LifecycleOwner owner, @NonNull Observer<byte[]> observer) {
        mMockData.observe(owner, observer);
    }

    @Override
    public void observeSavedData(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        mSavedData.observe(owner, observer);
    }

    @MainThread
    public void updateDeviceSettingName(@Nullable String text) {
        mDeviceNameSetting.setValue(text);
    }

    @MainThread
    public void updateMockData(@Nullable byte[] data) {
        mDisposable.add(Completable.create(emitter -> {
            mMockData.postValue(data);
            emitter.onComplete();
        }).subscribeOn(Schedulers.io())
                .subscribe());
    }

    public void fragmentReady() {
        mFragmentReady.postValue(true);
    }

    @NonNull
    @MainThread
    public Fragment getFragment(@NonNull Intent intent) {
        int deviceType = intent.getIntExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_UNDEFINED);
        if (DEVICE_TYPE_BLOOD_PRESSURE_PROFILE == deviceType) {
            return new BloodPressureProfileFragment();
        } else {
            throw new RuntimeException("No Data");
        }
    }

    @NonNull
    @MainThread
    public Class<? extends BaseSettingFragmentViewModel> getFragmentViewModelClass(@NonNull Intent intent) {
        int deviceType = intent.getIntExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_UNDEFINED);
        if (DEVICE_TYPE_BLOOD_PRESSURE_PROFILE == deviceType) {
            return BloodPressureProfileViewModel.class;
        } else {
            throw new RuntimeException("No Data");
        }
    }

    @Override
    public void save(@NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Single.<DeviceSetting>create(emitter -> {
            DeviceSetting deviceSetting = mDeviceSetting;
            if (deviceSetting == null) {
                emitter.onError(new RuntimeException("Already saved"));
            } else {
                String deviceNameSetting = mDeviceNameSetting.getValue();
                byte[] mockData = mMockData.getValue();

                if (deviceNameSetting != null && mDeviceSettingRepository.getDeviceSettingNameErrorString(deviceNameSetting) == null
                        && mockData != null) {
                    deviceSetting.setDeviceSettingName(deviceNameSetting);
                    deviceSetting.setDeviceSettingData(mockData);
                    mDeviceSetting = null;
                    emitter.onSuccess(deviceSetting);
                } else {
                    emitter.onError(new RuntimeException("Validation failed"));
                }
            }
        }).subscribeOn(Schedulers.io())
                .flatMapCompletable(mDeviceSettingRepository::insertDeviceSetting)
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(() -> mSavedData.setValue(true)
                        , onError));
    }

    @Override
    public void dispose() {
        mFragmentReady.setValue(false);
        super.dispose();
    }

}