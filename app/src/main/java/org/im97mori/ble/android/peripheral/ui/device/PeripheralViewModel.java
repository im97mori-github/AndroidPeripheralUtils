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

import com.google.gson.Gson;

import org.im97mori.ble.MockData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;
import org.im97mori.ble.android.peripheral.ui.BaseViewModel;
import org.im97mori.ble.profile.peripheral.AbstractProfileMockCallback;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class PeripheralViewModel extends BaseViewModel {

    private static final String KEY_TITLE = "KEY_TITLE";
    private static final String KEY_DEVICE_TYPE_IMAGE_RES = "KEY_DEVICE_TYPE_IMAGE_RES";
    private static final String KEY_DEVICE_TYPE = "KEY_DEVICE_TYPE";
    private static final String KEY_IS_READY = "KEY_IS_READY";
    private static final String KEY_IS_STARTED = "KEY_IS_STARTED";

    private final SavedStateHandle mSavedStateHandle;
    private final DeviceSettingRepository mDeviceSettingRepository;
    private final Gson mGson;

    private AbstractProfileMockCallback mAbstractProfileMockCallback;

    @Inject
    public PeripheralViewModel(@NonNull SavedStateHandle savedStateHandle
            , @NonNull DeviceSettingRepository deviceSettingRepository
            , @NonNull Gson gson) {
        mSavedStateHandle = savedStateHandle;
        mDeviceSettingRepository = deviceSettingRepository;
        mGson = gson;

        savedStateHandle.set(KEY_IS_READY, false);
    }

    @MainThread
    public void observeSetup(@NonNull Intent intent
            , @NonNull Action onSuccess
            , @NonNull Consumer<? super Throwable> onError) {
        if (mAbstractProfileMockCallback == null) {
            mDisposable.add(mDeviceSettingRepository
                    .loadDeviceSettingById(intent.getLongExtra(KEY_DEVICE_ID, VALUE_DEVICE_ID_UNSAVED))
                    .subscribeOn(Schedulers.io())
                    .flatMap(deviceSetting -> {
                        mSavedStateHandle.<String>getLiveData(KEY_TITLE).postValue(deviceSetting.getDeviceSettingName());
                        mSavedStateHandle.<Integer>getLiveData(KEY_DEVICE_TYPE_IMAGE_RES).postValue(mDeviceSettingRepository.getDeviceTypeImageResId(deviceSetting.getDeviceType()));
                        mSavedStateHandle.<Integer>getLiveData(KEY_DEVICE_TYPE).postValue(deviceSetting.getDeviceType());
                        return Single.just(mDeviceSettingRepository.createProfileMockCallback(deviceSetting.getDeviceType()
                                , mGson.fromJson(deviceSetting.getDeviceSettingData(), MockData.class)));
                    })
                    .observeOn(AndroidSchedulers.mainThread())
                    .flatMapCompletable(abstractProfileMockCallback -> {
                        mAbstractProfileMockCallback = abstractProfileMockCallback;
                        mSavedStateHandle.<Boolean>getLiveData(KEY_IS_READY).postValue(isPeripheralReady());
                        mSavedStateHandle.<Boolean>getLiveData(KEY_IS_STARTED).postValue(mAbstractProfileMockCallback.isStarted());
                        return Completable.complete();
                    })
                    .subscribe(onSuccess, onError));
        }
    }

    @MainThread
    public void observeDeleteDeviceSetting(@NonNull Intent intent, @NonNull Action onComplete) {
        mDisposable.add(mDeviceSettingRepository
                .deleteDeviceSetting(new DeviceSetting(intent.getLongExtra(KEY_DEVICE_ID, VALUE_DEVICE_ID_UNSAVED)))
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(onComplete));
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
    public void observeDeviceType(@NonNull LifecycleOwner owner, @NonNull Observer<Integer> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<Integer>getLiveData(KEY_DEVICE_TYPE)).observe(owner, observer);
    }

    @MainThread
    public void observeDeviceTypeName(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<Integer>getLiveData(KEY_DEVICE_TYPE)).observe(owner
                , integer -> observer.onChanged(mDeviceSettingRepository.getDeviceTypeName(integer)));
    }

    @MainThread
    public void observeIsReady(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<Boolean>getLiveData(KEY_IS_READY)).observe(owner, observer);
    }

    @MainThread
    public void observeIsStarted(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<Boolean>getLiveData(KEY_IS_STARTED)).observe(owner, observer);
    }

    @MainThread
    public boolean isPeripheralReady() {
        return mAbstractProfileMockCallback != null;
    }

    @MainThread
    public boolean isPeripheralStarted() {
        return mAbstractProfileMockCallback != null && mAbstractProfileMockCallback.isStarted();
    }

    @MainThread
    public void start() {
        if (mAbstractProfileMockCallback != null && !mAbstractProfileMockCallback.isStarted()) {
            mAbstractProfileMockCallback.start();
            mSavedStateHandle.<Boolean>getLiveData(KEY_IS_STARTED).postValue(mAbstractProfileMockCallback.isStarted());
        }
    }

    @MainThread
    public void quit() {
        if (isPeripheralStarted()) {
            mAbstractProfileMockCallback.quit();
            mSavedStateHandle.<Boolean>getLiveData(KEY_IS_STARTED).postValue(mAbstractProfileMockCallback.isStarted());
        }
    }

    @MainThread
    public void clear() {
        quit();
        mAbstractProfileMockCallback = null;
        mSavedStateHandle.<Boolean>getLiveData(KEY_IS_READY).postValue(isPeripheralReady());
    }

}