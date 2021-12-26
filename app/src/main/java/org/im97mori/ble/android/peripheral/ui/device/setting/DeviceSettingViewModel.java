package org.im97mori.ble.android.peripheral.ui.device.setting;

import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_ID;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.VALUE_DEVICE_ID_UNSAVED;

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
import org.im97mori.ble.android.peripheral.room.DeviceSetting;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.BaseDeviceSettingFragment;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp.BloodPressureProfileFragment;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.processors.PublishProcessor;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class DeviceSettingViewModel extends ViewModel {

    private static final String KEY_DEVICE_TYPE_IMAGE_RES_ID = "KEY_DEVICE_TYPE_IMAGE_RES_ID";
    private static final String KEY_DEVICE_TYPE_NAME = "KEY_DEVICE_TYPE_NAME";
    private static final String KEY_DEVICE_SETTING_NAME = "KEY_DEVICE_SETTING_NAME";
    private static final String KEY_FRAGMENT_READY = "KEY_FRAGMENT_READY";

    private final SavedStateHandle mSavedStateHandle;
    private final DeviceRepository mDeviceRepository;
    private final Gson mGson;

    private final MutableLiveData<String> mDeviceNameSetting;

    private final PublishProcessor<MockData> mMockDataPublishProcessor = PublishProcessor.create();

    private DeviceSetting mDeviceSetting;

    private BaseDeviceSettingFragment mBaseDeviceSettingFragment;

    @Inject
    public DeviceSettingViewModel(@NonNull SavedStateHandle savedStateHandle, @NonNull DeviceRepository deviceRepository, @NonNull Gson gson) {
        mSavedStateHandle = savedStateHandle;
        mDeviceRepository = deviceRepository;
        mGson = gson;
        mDeviceNameSetting = savedStateHandle.getLiveData(KEY_DEVICE_SETTING_NAME);
    }

    @NonNull
    @MainThread
    public Completable setup(@NonNull Intent intent) {
        return Single.just(intent.getLongExtra(KEY_DEVICE_ID, VALUE_DEVICE_ID_UNSAVED))
                .flatMap(id -> {
                    if (VALUE_DEVICE_ID_UNSAVED == id) {
                        return Single.just(new DeviceSetting("", intent.getIntExtra(Constants.IntentKey.KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE)));
                    } else {
                        return mDeviceRepository.loadDeviceSettingByIdAsSingle(id);
                    }
                })
                .subscribeOn(Schedulers.io())
                .flatMapCompletable(device -> {
                    // for 1st onStart
                    if (mDeviceSetting == null) {
                        mDeviceSetting = device;

                        // for Activity create
                        MutableLiveData<String> liveData = mSavedStateHandle.getLiveData(KEY_DEVICE_TYPE_NAME);
                        if (liveData.getValue() == null) {
                            liveData.postValue(mDeviceRepository.getDeviceTypeName(mDeviceSetting.getDeviceType()));
                            mSavedStateHandle.<Integer>getLiveData(KEY_DEVICE_TYPE_IMAGE_RES_ID).postValue(mDeviceRepository.getDeviceTypeImageResId(mDeviceSetting.getDeviceType()));
                            mDeviceNameSetting.postValue(mDeviceSetting.getDeviceSettingName());

                            MockData mockData = null;
                            try {
                                mockData = mGson.fromJson(mDeviceSetting.getDeviceSettingData(), MockData.class);
                            } catch (JsonSyntaxException e) {
                                e.printStackTrace();
                            } finally {
                                if (mockData == null) {
                                    mockData = new MockData();
                                }
                            }
                            mMockDataPublishProcessor.onNext(mockData);
                        }
                        return Completable.complete();
                    } else {
                        return Completable.error(new RuntimeException("Initialized"));
                    }
                })
                .observeOn(AndroidSchedulers.mainThread());
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
                , s -> observer.onChanged(mDeviceRepository.getDeviceSettingNameErrorString(s)));
    }

    @MainThread
    public void observeFragmentReady(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<Boolean>getLiveData(KEY_FRAGMENT_READY)).observe(owner, observer);
    }

    @MainThread
    public Single<MockData> observeMockData() {
        return mMockDataPublishProcessor.firstOrError().subscribeOn(Schedulers.io());
    }

    @MainThread
    public void updateDeviceSettingName(@Nullable String text) {
        mDeviceNameSetting.setValue(text);
    }

    public void fragmentReady() {
        mSavedStateHandle.<Boolean>getLiveData(KEY_FRAGMENT_READY).postValue(true);
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
        return Single.<DeviceSetting>create(emitter -> {
            DeviceSetting deviceSetting = mDeviceSetting;
            if (deviceSetting == null) {
                emitter.onError(new RuntimeException("Already saved"));
            } else {
                String deviceNameSetting = mDeviceNameSetting.getValue();
                String deviceNameSettingErrorString = mDeviceRepository.getDeviceSettingNameErrorString(deviceNameSetting);
                String moduleDataJson = mBaseDeviceSettingFragment.getModuleDataJson();
                if (!TextUtils.isEmpty(deviceNameSetting)
                        && TextUtils.isEmpty(deviceNameSettingErrorString)
                        && !TextUtils.isEmpty(moduleDataJson)) {
                    deviceSetting.setDeviceSettingName(deviceNameSetting);
                    deviceSetting.setDeviceSettingData(moduleDataJson);
                    mDeviceSetting = null;
                    emitter.onSuccess(deviceSetting);
                } else {
                    emitter.onError(new RuntimeException("Validation failed"));
                }
            }
        }).subscribeOn(Schedulers.io())
                .flatMapCompletable(mDeviceRepository::insertDeviceSetting)
                .observeOn(AndroidSchedulers.mainThread());
    }

}