package org.im97mori.ble.android.peripheral.ui.device.setting.u2a24;

import static org.im97mori.ble.constants.CharacteristicUUID.MODEL_NUMBER_STRING_CHARACTERISTIC;

import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattCharacteristic;
import android.content.Intent;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;
import androidx.lifecycle.SavedStateHandle;
import androidx.lifecycle.Transformations;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;
import org.im97mori.ble.android.peripheral.ui.device.setting.BaseCharacteristicViewModel;
import org.im97mori.ble.characteristic.u2a24.ModelNumberString;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class ModelNumberStringSettingViewModel extends BaseCharacteristicViewModel {

    private static final String KEY_IS_ERROR_RESPONSE = "KEY_IS_ERROR_RESPONSE";

    private static final String KEY_MODEL_NUMBER_NAME_STRING = "KEY_MODEL_NUMBER_NAME_STRING";
    private static final String KEY_RESPONSE_CODE = "KEY_RESPONSE_CODE";
    private static final String KEY_RESPONSE_DELAY = "KEY_RESPONSE_DELAY";

    private final MutableLiveData<Boolean> mIsErrorResponse;

    private final MutableLiveData<String> mModelNumberString;
    private final MutableLiveData<String> mResponseCode;
    private final MutableLiveData<String> mResponseDelay;

    @Inject
    public ModelNumberStringSettingViewModel(@NonNull SavedStateHandle savedStateHandle
            , @NonNull DeviceSettingRepository deviceSettingRepository
            , @NonNull Gson gson) {
        super(deviceSettingRepository, gson);

        mIsErrorResponse = savedStateHandle.getLiveData(KEY_IS_ERROR_RESPONSE);
        mModelNumberString = savedStateHandle.getLiveData(KEY_MODEL_NUMBER_NAME_STRING);
        mResponseCode = savedStateHandle.getLiveData(KEY_RESPONSE_CODE);
        mResponseDelay = savedStateHandle.getLiveData(KEY_RESPONSE_DELAY);
    }

    @Override
    public void observeSetup(@NonNull Intent intent
            , @NonNull Action onComplete
            , @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Completable.create(emitter -> {
            if (mCharacteristicData == null) {
                try {
                    mCharacteristicData = mGson.fromJson(intent.getStringExtra(MODEL_NUMBER_STRING_CHARACTERISTIC.toString())
                            , CharacteristicData.class);
                } catch (JsonSyntaxException e) {
                    e.printStackTrace();
                }

                if (mCharacteristicData == null) {
                    mCharacteristicData = new CharacteristicData();
                    mCharacteristicData.uuid = MODEL_NUMBER_STRING_CHARACTERISTIC;
                    mCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
                    mCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
                }

                if (mIsErrorResponse.getValue() == null) {
                    mIsErrorResponse.postValue(mCharacteristicData.responseCode != BluetoothGatt.GATT_SUCCESS);
                }

                if (mModelNumberString.getValue() == null) {
                    if (mCharacteristicData.data != null) {
                        mModelNumberString.postValue(new ModelNumberString(mCharacteristicData.data).getModelNumber());
                    }
                }

                if (mResponseCode.getValue() == null) {
                    mResponseCode.postValue(String.valueOf(mCharacteristicData.responseCode));
                }

                if (mResponseDelay.getValue() == null) {
                    mResponseDelay.postValue(String.valueOf(mCharacteristicData.delay));
                }

                emitter.onComplete();
            } else {
                emitter.onError(new RuntimeException("Initialized"));
            }
        })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(onComplete, onError));
    }

    @MainThread
    public void observeIsErrorResponse(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIsErrorResponse).observe(owner, observer);
    }

    @MainThread
    public void observeModelNumberString(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mModelNumberString).observe(owner, observer);
    }

    @MainThread
    public void observeModelNumberStringError(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mModelNumberString).observe(owner
                , s -> observer.onChanged(mDeviceSettingRepository.getModelNumberErrorString(s)));
    }

    @MainThread
    public void observeResponseCode(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mResponseCode).observe(owner, observer);
    }

    @MainThread
    public void observeResponseCodeError(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mResponseCode).observe(owner
                , s -> observer.onChanged(mDeviceSettingRepository.getResponseCodeErrorString(s)));
    }

    @MainThread
    public void observeResponseDelay(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mResponseDelay).observe(owner, observer);
    }

    @MainThread
    public void observeResponseDelayError(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mResponseDelay).observe(owner
                , s -> observer.onChanged(mDeviceSettingRepository.getResponseDelayErrorString(s)));
    }

    @MainThread
    public void updateIsErrorResponse(@NonNull Boolean checked) {
        mIsErrorResponse.setValue(checked);
    }

    @MainThread
    public void updateModelNumberStringString(@NonNull String text) {
        mModelNumberString.setValue(text);
    }

    @MainThread
    public void updateResponseCode(@NonNull String text) {
        mResponseCode.setValue(text);
    }

    @MainThread
    public void updateResponseDelay(@NonNull String text) {
        mResponseDelay.setValue(text);
    }

    @Override
    public void observeSave(@NonNull Consumer<Intent> onSuccess
            , @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Single.<Intent>create(emitter -> {
            CharacteristicData characteristicData = mCharacteristicData;
            if (characteristicData == null) {
                emitter.onError(new RuntimeException("Already saved"));
            } else {
                boolean isErrorResponse = Boolean.TRUE.equals(mIsErrorResponse.getValue());
                String responseCode = mResponseCode.getValue();
                String responseDelay = mResponseDelay.getValue();
                String modelNumberString = mModelNumberString.getValue();

                if (responseDelay != null && mDeviceSettingRepository.getResponseDelayErrorString(responseDelay) == null) {
                    characteristicData.delay = Long.parseLong(responseDelay);
                    if (isErrorResponse) {
                        if (responseCode != null && mDeviceSettingRepository.getResponseCodeErrorString(responseCode) == null) {
                            characteristicData.data = null;
                            characteristicData.responseCode = Integer.parseInt(responseCode);

                            Intent intent = new Intent();
                            intent.putExtra(MODEL_NUMBER_STRING_CHARACTERISTIC.toString(), mGson.toJson(characteristicData));
                            emitter.onSuccess(intent);
                        } else {
                            emitter.onError(new RuntimeException("Validation failed"));
                        }
                    } else {
                        if (modelNumberString != null && mDeviceSettingRepository.getModelNumberErrorString(modelNumberString) == null) {
                            characteristicData.data = new ModelNumberString(modelNumberString).getBytes();
                            characteristicData.responseCode = BluetoothGatt.GATT_SUCCESS;

                            Intent intent = new Intent();
                            intent.putExtra(MODEL_NUMBER_STRING_CHARACTERISTIC.toString(), mGson.toJson(characteristicData));

                            mCharacteristicData = null;
                            emitter.onSuccess(intent);
                        } else {
                            emitter.onError(new RuntimeException("Validation failed"));
                        }
                    }
                } else {
                    emitter.onError(new RuntimeException("Validation failed"));
                }
            }
        })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(onSuccess, onError));
    }
}