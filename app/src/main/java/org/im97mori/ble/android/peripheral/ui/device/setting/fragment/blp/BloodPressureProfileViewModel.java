package org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp;

import static org.im97mori.ble.constants.ServiceUUID.BLOOD_PRESSURE_SERVICE;
import static org.im97mori.ble.constants.ServiceUUID.DEVICE_INFORMATION_SERVICE;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;
import androidx.lifecycle.SavedStateHandle;
import androidx.lifecycle.Transformations;

import org.im97mori.ble.MockData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.BaseSettingFragmentViewModel;
import org.im97mori.ble.android.peripheral.utils.ExistObserver;
import org.im97mori.ble.android.peripheral.utils.Utils;

import java.util.LinkedList;
import java.util.Optional;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class BloodPressureProfileViewModel extends BaseSettingFragmentViewModel {

    private static final String KEY_IS_DIS_SUPPORTED = "KEY_IS_DIS_SUPPORTED";

    private static final String KEY_BLS_DATA = "KEY_BLS_DATA";
    private static final String KEY_DIS_DATA = "KEY_DIS_DATA";

    private MockData mMockData;

    private final MutableLiveData<Boolean> mIsDisSupported;

    private final MutableLiveData<byte[]> mBlsData;
    private final MutableLiveData<byte[]> mDisData;

    private final MutableLiveData<byte[]> mSavedData;

    @Inject
    public BloodPressureProfileViewModel(@NonNull SavedStateHandle savedStateHandle
            , @NonNull DeviceSettingRepository deviceSettingRepository) {
        super(deviceSettingRepository);
        mIsDisSupported = savedStateHandle.getLiveData(KEY_IS_DIS_SUPPORTED);
        mBlsData = savedStateHandle.getLiveData(KEY_BLS_DATA);
        mDisData = savedStateHandle.getLiveData(KEY_DIS_DATA);

        mSavedData = savedStateHandle.getLiveData(KEY_SAVED_DATA);
    }

    public void observeSetup(@Nullable byte[] data
            , @NonNull Action onComplete
            , @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Completable.create(emitter -> {
            if (mMockData == null) {
                mMockData = Utils.byteToParcelable(data, MockData.CREATOR);

                if (mMockData == null) {
                    mMockData = new MockData(new LinkedList<>());
                }

                Optional<ServiceData> blsServiceDataOptional = mMockData.serviceDataList
                        .stream()
                        .filter(serviceData -> serviceData.uuid.equals(BLOOD_PRESSURE_SERVICE))
                        .findAny();

                Optional<ServiceData> disServiceDataOptional = mMockData.serviceDataList
                        .stream()
                        .filter(serviceData -> serviceData.uuid.equals(DEVICE_INFORMATION_SERVICE))
                        .findAny();

                if (mIsDisSupported.getValue() == null) {
                    mIsDisSupported.postValue(disServiceDataOptional.isPresent());
                }

                if (mBlsData.getValue() == null && blsServiceDataOptional.isPresent()) {
                    mBlsData.postValue(Utils.parcelableToByteArray(blsServiceDataOptional.get()));
                }

                if (mDisData.getValue() == null && disServiceDataOptional.isPresent()) {
                    mDisData.postValue(Utils.parcelableToByteArray(disServiceDataOptional.get()));
                }
            }
            emitter.onComplete();
        }).subscribeOn(Schedulers.io())
                .subscribe(onComplete, onError));
    }

    public void observeHasBlsData(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mBlsData).observe(owner, new ExistObserver(observer));
    }

    public void observeHasDisData(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mDisData).observe(owner, new ExistObserver(observer));
    }

    public void observeIsDisSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIsDisSupported).observe(owner, observer);
    }

    @Override
    public void observeSavedData(@NonNull LifecycleOwner owner, @NonNull Observer<byte[]> observer) {
        mSavedData.observe(owner, observer);
    }

    @MainThread
    public void updateIsDisSupported(boolean checked) {
        mIsDisSupported.setValue(checked);
    }

    @Nullable
    @MainThread
    public byte[] getBlsData() {
        return mBlsData.getValue();
    }

    @MainThread
    public void setBlsData(@Nullable byte[] blsData) {
        mBlsData.setValue(blsData);
    }

    @Nullable
    @MainThread
    public byte[] getDisData() {
        return mDisData.getValue();
    }

    @MainThread
    public void setDisData(@Nullable byte[] disData) {
        mDisData.setValue(disData);
    }

    @Override
    public void save(@NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Completable.create(emitter -> {
            byte[] blsData = mBlsData.getValue();
            boolean isDisSupported = Boolean.TRUE.equals(mIsDisSupported.getValue());
            byte[] disData = mDisData.getValue();

            if (blsData != null && (!isDisSupported || disData != null)) {
                mMockData.serviceDataList.clear();
                mMockData.serviceDataList.add(Utils.byteToParcelable(blsData, ServiceData.CREATOR));
                if (disData != null) {
                    mMockData.serviceDataList.add(Utils.byteToParcelable(disData, ServiceData.CREATOR));
                }

                mSavedData.postValue(Utils.parcelableToByteArray(mMockData));
                mMockData = null;
                emitter.onComplete();
            } else {
                emitter.onError(new RuntimeException("Validation failed"));
            }
        })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(() -> {
                }, onError));
    }

}