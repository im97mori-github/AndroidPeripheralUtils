package org.im97mori.ble.android.peripheral.ui.device;

import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_ID;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.VALUE_DEVICE_ID_UNSAVED;

import android.content.Intent;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.Observer;
import androidx.lifecycle.SavedStateHandle;
import androidx.lifecycle.Transformations;
import androidx.lifecycle.ViewModel;

import com.google.gson.Gson;

import org.im97mori.ble.MockData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;
import org.im97mori.ble.profile.peripheral.AbstractProfileMockCallback;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class PeripheralViewModel extends ViewModel {

    private static final String KEY_TITLE = "KEY_TITLE";
    private static final String KEY_DEVICE_TYPE_IMAGE_RES = "KEY_DEVICE_TYPE_IMAGE_RES";
    private static final String KEY_DEVICE_TYPE_NAME = "KEY_DEVICE_TYPE_NAME";

    private final DeviceSettingRepository mDeviceSettingRepository;

    private final Gson mGson;

    private final SavedStateHandle mSavedStateHandle;

    @Inject
    public PeripheralViewModel(@NonNull SavedStateHandle savedStateHandle, @NonNull DeviceSettingRepository deviceSettingRepository, @NonNull Gson gson) {
        mDeviceSettingRepository = deviceSettingRepository;
        mGson = gson;
        mSavedStateHandle = savedStateHandle;
    }

    @NonNull
    @MainThread
    public Single<AbstractProfileMockCallback> setup(@NonNull Intent intent) {
        return mDeviceSettingRepository
                .loadDeviceSettingById(intent.getLongExtra(KEY_DEVICE_ID, VALUE_DEVICE_ID_UNSAVED))
                .subscribeOn(Schedulers.io())
                .flatMap(device -> {
                    mSavedStateHandle.<String>getLiveData(KEY_TITLE).postValue(device.getDeviceSettingName());
                    mSavedStateHandle.<Integer>getLiveData(KEY_DEVICE_TYPE_IMAGE_RES).postValue(mDeviceSettingRepository.getDeviceTypeImageResId(device.getDeviceType()));
                    mSavedStateHandle.<String>getLiveData(KEY_DEVICE_TYPE_NAME).postValue(mDeviceSettingRepository.getDeviceTypeName(device.getDeviceType()));
                    return Single.just(mDeviceSettingRepository.createProfileMockCallback(device.getDeviceType()
                            , mGson.fromJson(device.getDeviceSettingData(), MockData.class)));
                })
                .observeOn(AndroidSchedulers.mainThread());
    }

    @NonNull
    @MainThread
    public Completable deleteDevice(@NonNull Intent intent) {
        return Single.just(intent.getLongExtra(KEY_DEVICE_ID, VALUE_DEVICE_ID_UNSAVED))
                .subscribeOn(Schedulers.io())
                .flatMapCompletable(id -> mDeviceSettingRepository.deleteDeviceSetting(new DeviceSetting(id)))
                .subscribeOn(AndroidSchedulers.mainThread());
    }

    @MainThread
    public void observeTypeImageRes(@NonNull LifecycleOwner owner, @NonNull Observer<Integer> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<Integer>getLiveData(KEY_DEVICE_TYPE_IMAGE_RES)).observe(owner, observer);
    }

    @MainThread
    public void observeTitle(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_TITLE)).observe(owner, observer);
    }

    @MainThread
    public void observeDeviceTypeName(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_DEVICE_TYPE_NAME)).observe(owner, observer);
    }

}